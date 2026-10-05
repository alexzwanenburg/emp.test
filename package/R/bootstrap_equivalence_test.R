#' @include generic_test.R
#' @include bootstrap_difference_test.R
NULL

#' Bootstrap equivalence test
#'
#' Performs a two-sample equivalence test on variables.
#'
#' @param ... Two or more numeric variables that are to be tested. If exactly
#'   two variables are provided, the first variable is used as the control
#'   group. The computed difference are between test and control groups (test -
#'   control). If `paired = FALSE` (default), the number of values in each
#'   variable can be different. If `paired = TRUE`, the number of values in each
#'   variable must be the same.
#'
#'   If more than two numeric variables are provided, tests are conducted in
#'   pairwise combinations. In this case it is recommended to set symmetric
#'   `tolerance` boundaries or generate this dynamically.
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
#'   **first two** arguments of the function are expected to be single numeric values
#'   that were computed after applying `summary_function` to the a pair of
#'   columns of the input data. This allows for dynamically computing tolerance
#'   values based on summary statistic values.
#' @param tolerance_function_args A named list of additional arguments supplied
#'   to `tolerance`, if `tolerance` is a custom function.
#' @param alternative The alternative hypothesis. One of `"equivalent"` (default),
#'   `"not_inferior"` or `"not_superior"`.
#' @inheritParams difference_test
#'
#' @details
#' An equivalence test assesses the difference between two variables (samples),
#' but from a different starting assumption than a difference test. Whereas
#' difference tests assume that there is no difference between groups, an
#' equivalence test assumes that there a non-marginal difference exists. That is,
#' it tests for the absence of a meaningful difference.
#'
#' Equivalence tests are often performed using the two one-sided test (TOST)
#' procedure. The bootstrap equivalence test generalises such tests by allowing
#' a custom `summary_function`, dynamic tolerances, and not having any
#' assumptions except for independence.
#'
#' The equivalence test (`alternative = "equivalent"`) has two null hypotheses:
#' \eqn{H_{0a}: f(x_t) - f(x_c) \leq \delta_L}{H0a: f(x2) - f(x1) ≤ ΔL} or
#' \eqn{H_{0b}: f(x_t) - f(x_c) \geq \delta_U}{H0a: f(x2) - f(x1) ≥ ΔU}.
#' Here \eqn{f(x_c)}{f(x1)} and \eqn{f(x_t)}{f(x2)} are the summary descriptors
#' of control variable \eqn{x_c}{x1} and target variable \eqn{x_t}{x2}, respectively.
#' \eqn{\delta_L}{ΔL} and \eqn{\delta_U}{ΔU} are the lower and upper tolerances.
#'
#' The alternative hypothesis is \eqn{H_1: \delta_L < f(x_t) - f(x_c) < \delta_U}{H1: ΔL < f(x2) - f(x1) < ΔU}.
#'
#' An equivalence test with `alternative = "not_inferior"` is a non-inferiority
#' test. The null hypothesis is \eqn{H_{0a}}{H0a}, and the alternative hypothesis
#' is \eqn{H_1: \delta_L < f(x_t) - f(x_c)}{H1: ΔL < f(x2) - f(x1)}.
#'
#' An equivalence test with `alternative = "not_superior"` is a non-superiority
#' test. The null hypothesis is \eqn{H_{0b}}{H0b}, and the alternative hypothesis
#' is \eqn{H_1: f(x_t) - f(x_c) < \delta_U}{H1: f(x2) - f(x1) < ΔU}.
#'
#' @returns htest object
#' @export
equivalence_test <- function(
    ...,
    tolerance,
    tolerance_function_args = NULL,
    alternative = "equivalent",
    significance_level = 0.95,
    paired = FALSE,
    summary_function = mean,
    summary_function_args = NULL
) {
  if (paired) {
    # Create test object for paired difference tests.
    object <- methods::new(
      "empiricalPairedEquivalenceTest",
      data = list(...),
      tolerance = tolerance,
      tolerance_function_args = tolerance_function_args,
      alternative = alternative,
      significance_level = significance_level,
      summary_function = summary_function,
      summary_function_args = summary_function_args
    )

  } else {
    # Create test object for difference tests without paired data.
    object <- methods::new(
      "empiricalEquivalenceTest",
      data = list(...),
      tolerance = tolerance,
      tolerance_function_args = tolerance_function_args,
      alternative = alternative,
      significance_level = significance_level,
      summary_function = summary_function,
      summary_function_args = summary_function_args
    )
  }

  # Set the number of bootstraps.
  object@n_bootstraps <- 1000L

  # Perform the test.
  results <- run_test(object)

  # Return test information.
  return(results@htest)
}



# empiricalEquivalenceTest object ----------------------------------------------
setClass(
  "empiricalEquivalenceTest",
  contains = "empiricalDiffTest",
  slots = list(
    tolerance = "ANY",
    tolerance_function_args = "ANY"
  ),
  prototype = list(
    # name is set within the initialize method (depends on alternative)
    alternative_values = c("equivalent", "not_inferior", "not_superior"),
    tolerance = NULL,
    tolerance_function_args = NULL
  )
)


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalEquivalenceTest"),
  function(
    .Object,
    tolerance,
    tolerance_function_args = NULL,
    ...
  ) {
    # Pass to empiricalTest for global attributes.
    .Object <- callNextMethod()

    if (rlang::is_missing(tolerance)) ..error_missing_variable("tolerance")

    # tolerance can be either a function or one or two numeric values (lower,
    # upper tolerance).
    if (rlang::is_function(tolerance)) {
      # Check that the function returns one or two values.
      y <- do.call(
        tolerance,
        args = c(
          list(.Object@input_data[[1L]][1L], .Object@input_data[[2L]][1L]),
          tolerance_function_args
        )
      )

      .check_is_numeric(
        x = y,
        var_name = "tolerance output"
      )
      .check_argument_length(
        x = y,
        var_name = "tolerance output",
        min = 1L,
        max = 2L
      )

      .Object@tolerance <- tolerance
      .Object@tolerance_function_args <- tolerance_function_args

    } else if (is.numeric(tolerance)) {
      # Check that tolerance consists of one or two numeric values.
      .check_argument_length(
        x = tolerance,
        var_name = "tolerance",
        min = 1L,
        max = 2L
      )

      .Object@tolerance <- tolerance

    } else {
      msg <- "tolerance must be one "
      if (.Object@alternative == "equivalent") {
        msg <- c(msg, "or two (lower, upper tolerance) numeric values ")
      } else if (.Object@alternative == "not_inferior") {
        msg <- c(msg, "(lower tolerance) numeric value ")
      } else if (.Object@alternative == "not_superior") {
        msg <- c(msg, "(upper tolerance) numeric value ")
      }
      msg <- c(msg, "or a function that yields such values.", collapse = "")

      rlang::abort(
        message = paste0(msg),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    # Set name
    .Object@name <- switch(
      .Object@alternative,
      "equivalent" = "bootstrap (two-sample) equivalence test",
      "not_inferior" = "bootstrap (two-sample) non-inferiority test",
      "not_superior" = "bootstrap (two-sample) non-superiority test"
    )

    return(.Object)
  }
)


## .set_percentiles ------------------------------------------------------------
setMethod(
  ".set_percentiles",
  signature(object = "empiricalEquivalenceTest"),
  function(
    object,
    ...
  ) {
    # For equivalence tests
    return(object)
  }
)



## .test -----------------------------------------------------------------------
setMethod(
  ".test",
  signature(object = "empiricalEquivalenceTest"),
  function(
    object,
    data,
    hypothesis_test_only = FALSE,
    ...
  ) {
    test_output <- list()

    # Iterate over combinations of groups.
    for (groups in combn(names(data), m = 2L, simplify = FALSE)) {
      results <- ..test(
        object = object,
        x_1 = data[[groups[1L]]],
        x_2 = data[[groups[2L]]],
        hypothesis_test_only = hypothesis_test_only
      )

      # Add control and test group.
      if (!hypothesis_test_only) {
        results$control_group <- groups[1L]
        results$test_group <- groups[2L]
      }

      test_output[[paste0(groups[2L], " - ", paste0(groups[1L]))]] <- results
    }

    return(test_output)
  }
)




## ..test -----------------------------------------------------------------------
setMethod(
  "..test",
  signature(object = "empiricalEquivalenceTest"),
  function(
    object,
    x_1,
    x_2,
    hypothesis_test_only = FALSE,
    ...
  ) {
    # Get tolerance bounds
    tolerance <- .get_tolerance_bounds(
      object = object,
      x_1 = x_1,
      x_2 = x_2
    )
    tolerance_lower <- tolerance$tolerance_lower
    tolerance_upper <- tolerance$tolerance_upper

    # Count the number of times we observe that observations according to the
    # null-hypothesis (the difference is outside of tolerance) are observed.
    if (object@alternative == "equivalent") {
      # For consistency with the two one-sided test (TOST) procedure, the
      # boundaries are tested separately.
      p_lower <- x_2 - x_1 <= tolerance_lower
      p_upper <- x_2 - x_1 >= tolerance_upper
      p_value <- max(sum(p_lower), sum(p_upper)) / length(p_lower)

    } else if (object@alternative == "not_inferior") {
      p <- (x_2 - x_1 <= tolerance_lower)
      p_value <- sum(p) / length(p)

    } else if (object@alternative == "not_superior") {
      p <- (x_2 - x_1 >= tolerance_upper)
      p_value <- sum(p) / length(p)

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }

    reject_null <- p_value <= 1.0 - object@significance_level

    if (hypothesis_test_only) {
      return(list("reject_null" = reject_null))
    }

    # Compute effect.
    effect <- stats::median(x_2 - x_1, type = 8L)

    return(list(
      "effect" = effect,
      "p_value" = p_value,
      "reject_null" = reject_null,
      "alternative" = object@alternative
    ))
  }
)



# empiricalPairedEquivalenceTest object ----------------------------------------
setClass(
  "empiricalPairedEquivalenceTest",
  contains = "empiricalEquivalenceTest",
  prototype = list(
    requires_summary_function = FALSE
  )
)



## initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalPairedEquivalenceTest"),
  function(
    .Object,
    ...
  ) {
    .Object <- callNextMethod()

    # Check that all data entries have the same length.
    .check_paired_input_data(data = .Object@input_data)

    # Set n_draw_input_samples, which determines the number of random numbers
    # that should be drawn for summaries.
    .Object@n_draw_input_samples <- get_n_draw_input_samples(.Object)

    # Set name
    .Object@name <- switch(
      .Object@alternative,
      "equivalent" = "paired (two-sample) bootstrap equivalence test",
      "not_inferior" = "paired (two-sample) bootstrap non-inferiority test",
      "not_superior" = "paired (two-sample) bootstrap non-superiority test"
    )
    browser()

    return(.Object)
  }
)


## get_n_draw_input_samples ----------------------------------------------------
setMethod(
  "get_n_draw_input_samples",
  signature(
    object = "empiricalPairedEquivalenceTest"
  ),
  function(object) {
    if (is.null(object@input_data)) return(0L)

    # The same samples should be drawn for each input.
    return(length(object@input_data[[1L]]))
  }
)


## get_n_draw_summary_samples method -------------------------------------------
setMethod(
  "get_n_draw_summary_samples",
  signature(object = "empiricalPairedEquivalenceTest"),
  function(object) {
    if (is.null(object@summary_data)) return(0L)

    # The same samples should be drawn for each input.
    return(length(object@summary_data[[1L]]))
  }
)


## .sample_data ----------------------------------------------------------------
setMethod(
  ".sample_data",
  signature(object = "empiricalPairedEquivalenceTest"),
  function(object, random_values, ...) {
    data <- object@summary_data
    if (is.null(data)) data <- object@input_data
    if (is.null(random_values)) return(data)

    # Special case for point estimates.
    if (is.null(random_values)) return(object@input_data)

    # For paired tests the same samples should be randomly selected.
    n_samples <- length(data[[1L]])
    sample_indices <- as.integer(ceiling(random_values[1L:n_samples] * n_samples))

    # These edge cases are highlu unlikely but not impossible.
    sample_indices[sample_indices < 1L] <- 1L
    sample_indices[sample_indices > n_samples] <- n_samples

    # Select samples.
    sampled_data <- list()
    for (variable in names(data)) {
      sampled_data[[variable]] <- data[[variable]][sample_indices]
    }

    return(sampled_data)
  }
)
