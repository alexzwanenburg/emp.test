#' @include generic_location_test.R
NULL

#' Bootstrap equivalence test
#'
#' Performs a one-sample equivalence test on variables.
#'
#' @param ... One or more numeric variables that are to be tested. If more than
#'   one numeric variable is provided, tests are conducted separately for each
#'   variable. In this case it is recommended to set symmetric `tolerance`
#'   boundaries or generate this dynamically.
#' @param tolerance Numeric values setting the lower and / or upper boundary for
#'   the test. If `alternative = "equivalent`, one or two values are expected.
#'   If one value is provided, the provided value is used as the upper boundary,
#'   and its negative counterpart is used as the lower boundary, that is, the
#'   boundaries are symmetric around 0. If `alternative = "not_inferior"`, one
#'   value is expected, which is used as the lower boundary. If `alternative =
#'   "not_superior"`, one value is expected, which is used as the upper
#'   boundary.
#'
#'   Instead of providing numeric values, a custom function can be provided that
#'   yields one or two numeric values (depending on `alternative`). The
#'   **first** argument of the function is expected to be a single numeric value
#'   that was computed after applying `summary_function` to a variable. This
#'   allows for dynamically computing tolerance values based on summary
#'   statistic values.
#' @param tolerance_function_args A named list of additional arguments supplied
#'   to `tolerance`, if `tolerance` is a custom function.
#' @param alternative The alternative hypothesis. One of `"equivalent"`
#'   (default), `"not_inferior"` or `"not_superior"`.
#' @param summary_function Function for computing the summary descriptor from an
#'   input variable. By default: `mean`, which means that mean value of an input
#'   variable is evaluated. The `summary_function` can be any function that
#'   yields a single numeric value for each input variable. The **first**
#'   argument of the `summary_function` is expected to be a single input
#'   variable.
#' @param summary_function_args A named list of arguments that is provided to
#'   `summary_function`.
#' @inheritParams difference_test
#'
#' @details An one-sample equivalence test assesses the location of a variable
#'   (sample), but from a different starting assumption than a difference test.
#'   Whereas difference tests assume that there is no difference between groups,
#'   an equivalence test assumes that a non-marginal difference exists. That is,
#'   it tests for the absence of a meaningful difference.
#'
#'   Equivalence tests are often performed using the two one-sided test (TOST)
#'   procedure. The bootstrap equivalence test generalises such tests by
#'   allowing a custom `summary_function`, dynamic tolerances, and not having
#'   any assumptions except for independence.
#'
#'   The one-sample equivalence test (`alternative = "equivalent"`) has two null
#'   hypotheses: \eqn{H_{0a}: f(x) \leq \delta_L}{H0a: f(x) ≤ ΔL} or
#'   \eqn{H_{0b}: f(x) \geq \delta_U}{H0a: f(x) ≥
#'   ΔU}. \eqn{\delta_L}{ΔL} and \eqn{\delta_U}{ΔU} are the lower and upper tolerances.
#'
#'   The alternative hypothesis is \eqn{H_1: \delta_L < f(x) < \delta_U}{H1: ΔL
#'   < f(x) < ΔU}.
#'
#'   An equivalence test with `alternative = "not_inferior"` is a
#'   non-inferiority test. The null hypothesis is \eqn{H_{0a}}{H0a}, and the
#'   alternative hypothesis is \eqn{H_1: \delta_L < f(x)}{H1: ΔL <
#'   f(x)}.
#'
#'   An equivalence test with `alternative = "not_superior"` is a
#'   non-superiority test. The null hypothesis is \eqn{H_{0b}}{H0b}, and the
#'   alternative hypothesis is \eqn{H_1: f(x) < \delta_U}{H1: f(x) < ΔU}.
#'
#' @returns htest object
#' @export
one_sample_equivalence_test <- function(
    ...,
    tolerance,
    tolerance_function_args = NULL,
    alternative = "equivalent",
    significance_level = 0.95,
    summary_function = mean,
    summary_function_args = NULL
) {
  # Create test object.
  object <- methods::new(
    "empiricalEquivalenceLocTest",
    data = list(...),
    tolerance = tolerance,
    tolerance_function_args = tolerance_function_args,
    alternative = alternative,
    significance_level = significance_level,
    summary_function = summary_function,
    summary_function_args = summary_function_args
  )

  # Set the number of bootstraps.
  object@n_bootstraps <- 1000L

  # Perform the test.
  results <- run_test(object)

  # Return test information.
  return(results@htest)
}



# empiricalEquivalenceTest object ----------------------------------------------
setClass(
  "empiricalEquivalenceLocTest",
  contains = "empiricalGenericLocTest",
  slots = list(
    tolerance = "ANY",
    tolerance_function_args = "ANY"
  ),
  prototype = list(
    # name is set within the initialize method (depends on alternative)
    alternative_values = c("equivalent", "not_inferior", "not_superior"),
    tolerance = NULL,
    tolerance_function_args = NULL,
    requires_summary_function = FALSE
  )
)


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalEquivalenceLocTest"),
  function(
    .Object,
    tolerance,
    tolerance_function_args = NULL,
    ...
  ) {
    # Pass to empiricalTest for global attributes.
    .Object <- callNextMethod()

    # Check specific conditions for data.
    .check_input_data(
      .Object@input_data,
      condition = c("at_least", "1")
    )

    # Set n_draw_input_samples, which determines the number of random numbers
    # that should be drawn for summaries,
    .Object@n_draw_input_samples <- get_n_draw_input_samples(.Object)

    # Set tolerance and function arguments, if any.
    .check_tolerance(
      object = .Object,
      tolerance = tolerance,
      tolerance_function_args = tolerance_function_args,
      x_1 = .Object@input_data[[1L]][1L]
    )
    .Object@tolerance <- tolerance
    .Object@tolerance_function_args <- tolerance_function_args

    # Set name
    .Object@name <- switch(
      .Object@alternative,
      "equivalent" = "bootstrap (one-sample) equivalence test",
      "not_inferior" = "bootstrap (one-sample) non-inferiority test",
      "not_superior" = "bootstrap (one-sample) non-superiority test"
    )

    return(.Object)
  }
)



## .set_percentiles ------------------------------------------------------------
setMethod(
  ".set_percentiles",
  signature(object = "empiricalEquivalenceLocTest"),
  function(
    object,
    ...
  ) {
    if (object@alternative == "equivalent") {
      # group 1 has either smaller or larger summary statistic than group 2
      # by default (0.05, 0.95)
      pctl <- c(1.0 - object@significance_level, object@significance_level)

    } else if (object@alternative == "not_inferior") {
      # by default (0.05)
      pctl <- 1.0 - object@significance_level

    } else if (object@alternative == "not_superior") {
      # by default (0.95)
      pctl <- object@significance_level

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }

    object@percentiles <- pctl

    return(object)
  }
)



## .test -----------------------------------------------------------------------
setMethod(
  ".test",
  signature(object = "empiricalEquivalenceLocTest"),
  function(
    object,
    data,
    hypothesis_test_only = FALSE,
    ...
  ) {
    test_output <- list()

    # Iterate over combinations of groups.
    for (group in names(data)) {
      results <- ..test(
        object = object,
        x_1 = data[[group]],
        hypothesis_test_only = hypothesis_test_only
      )
      test_output[[group]] <- results
    }

    return(test_output)
  }
)



## ..test -----------------------------------------------------------------------
setMethod(
  "..test",
  signature(object = "empiricalEquivalenceLocTest"),
  function(
    object,
    x_1,
    hypothesis_test_only = FALSE,
    ...
  ) {
    # Get tolerance bounds
    tolerance <- .get_tolerance_bounds(
      object = object,
      x_1 = x_1
    )
    tolerance_lower <- tolerance$tolerance_lower
    tolerance_upper <- tolerance$tolerance_upper

    return(..test_equivalence(
      object = object,
      x = x_1,
      hypothesis_test_only = hypothesis_test_only,
      tolerance_lower = tolerance_lower,
      tolerance_upper = tolerance_upper
    ))
  }
)


## .compute_test_results -------------------------------------------------------
setMethod(
  ".compute_test_results",
  signature(object = "empiricalEquivalenceLocTest"),
  function(
    object,
    x,
    ...
  ) {
    # Obtain results.
    results <- .test(object = object, data = object@summary_data)

    # Compute statistical power based on bootstrap tests.
    for (group in names(x)) {
      statistical_power <- sum(x[[group]]$reject_null) / length(x[[group]]$reject_null)
      results[[group]]$power <- statistical_power
    }

    return(results)
  }
)
