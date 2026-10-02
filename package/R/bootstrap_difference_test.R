#' @include generic_test.R
#' @include generic_location_test.R
NULL

#' Bootstrap difference test
#'
#' Performs a difference test on variables.
#'
#' @param ... Two or more numeric variables that are to be tested. If `paired =
#'   FALSE` (default), the number of values in each variable can be different.
#'   If `paired = TRUE`, the number of values in each variable must be the same.
#'
#'   If more than two numeric variables are provided, tests are conducted for
#'   pairwise combinations of the variables.
#' @param alternative The alternative hypothesis. One of `"two_sided"`, `"greater"`
#'   and `"less"`.
#' @param significance_level Significance level for rejecting the null hypothesis.
#'   The default value is 0.95, which means that the null hypothesis is rejected
#'   if the p-value of the test is 0.05 or smaller. Must be a number between
#'   0.0 and 1.0.
#' @param paired Indicator for paired tests. By default, data is considered to
#'   be unpaired (`paired = FALSE`). If paired `paired = TRUE`, all numeric
#'   variables need to have the same number of values.
#' @param summary_function Function for computing the summary descriptor from
#'   input variables. By default: `mean`, which means that mean values of the
#'   input variables are compared. The `summary_function` can be any function
#'   that yields a single numeric value for each input. The **first** argument
#'   of the `summary_function` is expected to be a single input variable.
#'
#'   If data are paired (`paired = TRUE`), `summary_function = NULL` is also
#'   valid. Difference tests are then computed from the input data directly
#'   instead of from summary descriptors.
#' @param summary_function_args A named list of arguments that is provided to
#'   `summary_function`.
#'
#' @details
#' A difference test assesses the difference between two variables (samples).
#' An example of a difference test is Student's t-test, which assesses the difference
#' in the means of two variables. The bootstrap difference test generalises such
#' tests by allowing a custom `summary_function`, and not having any assumptions
#' except for independence.
#'
#' The difference test has the null hypothesis
#' \eqn{H_0: f(x_1) = f(x_2)}{H0: f(x1) = f(x2)}. Here \eqn{f(x_1)}{f(x1)} and
#' \eqn{f(x_2)}{f(x2)} are the summary descriptors of input variables \eqn{x_1}{x1}
#' and \eqn{x_2}{x2}, respectively.
#'
#' The alternative hypothesis depends on `alternative`. When `alternative = "two_sided"`,
#' the alternative hypothesis is \eqn{H_1: f(x_1) \neq f(x_2)}{H1: f(x1) ≠ f(x2)}.
#' For `alternative = "greater"` the alternative hypothesis is
#' \eqn{H_1: f(x_1) > f(x_2)}{H1: f(x1) > f(x2)}.
#' Finally, for `alternative = "less"` the alternative hypothesis is
#' \eqn{H_1: f(x_1) < f(x_2)}{H1: f(x1) < f(x2)}.
#'
#' @returns htest object
#' @export
difference_test <- function(
    ...,
    alternative = "two_sided",
    significance_level = 0.95,
    paired = FALSE,
    summary_function = mean,
    summary_function_args = NULL
) {
  if (paired) {
    # Create test object for paired difference tests.
    object <- methods::new(
      "empiricalPairedDiffTest",
      data = list(...),
      alternative = alternative,
      significance_level = significance_level,
      summary_function = summary_function,
      summary_function_args = summary_function_args
    )

  } else {
    # Create test object for difference tests without paired data.
    object <- methods::new(
      "empiricalDiffTest",
      data = list(...),
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




# empiricalDiffTest object -----------------------------------------------------
setClass(
  "empiricalDiffTest",
  contains = "empiricalGenericLocTest",
  prototype = list(
    name = "bootstrap (two-sample) difference test",
    requires_summary_function = TRUE
  )
)


## initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalDiffTest"),
  function(
    .Object,
    ...
  ) {
    # Update with parent class first. This checks significance_level.
    .Object <- callNextMethod()

    # Check specific conditions for data.
    .check_input_data(
      .Object@input_data,
      condition = c("at_least", "2")
    )

    # Set n_draw_input_samples, which determines the number of random numbers that
    # should be drawn for summaries,
    .Object@n_draw_input_samples <- get_n_draw_input_samples(.Object)

    return(.Object)
  }
)

## .test ------------------------------------------------------------------------
setMethod(
  ".test",
  signature(object = "empiricalDiffTest"),
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
      test_output[[paste0(groups[1L], " - ", paste0(groups[2L]))]] <- results
    }

    return(test_output)
  }
)


## ..test -----------------------------------------------------------------------
setMethod(
  "..test",
  signature(object = "empiricalDiffTest"),
  function(
    object,
    x_1,
    x_2,
    hypothesis_test_only = FALSE,
    ...
  ) {
    # Compute difference between groups. Pre-sort so that both quantile
    # and
    x <- sort(x_1 - x_2)

    # Compute confidence interval.
    ci <- stats::quantile(x, probs = object@percentiles, type = 8L, names = FALSE)

    # Early break in case the confidence interval is singular.
    if (length(ci) == 2L) {
      if (diff(ci) == 0.0) {
        if (hypothesis_test_only) {
          return(list("reject_null" = FALSE))

        } else {
          return(list(
            "effect" = ci[1L],
            "confidence_interval" = c(NA_real_, NA_real_),
            "p_value" = NA_real_,
            "reject_null" = FALSE,
            "alternative" = object@alternative
          ))
        }
      }
    }

    if (hypothesis_test_only) {
      # Test null hypothesis and compute 0-value.
      if (object@alternative == "two_sided") {
        reject_null <- ci[1L] >= 0.0 || (ci[2L] <= 0.0)

      } else if (object@alternative == "less") {
        reject_null <- ci[1L] <= 0.0

      } else if (object@alternative == "greater") {
        reject_null <- ci[1L] >= 0.0

      } else {
        ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
      }
      return(list("reject_null" = reject_null))
    }

    # Compute effect..
    effect <- stats::median(x, type = 8L)

    # Compute percentiles on x. Note that this corresponds to type = 8.
    p <- (seq_along(x) - 1.0 / 3.0) / (length(x) + 1.0 /3.0)
    p_value <- suppressWarnings(
      stats::approx(x = x, y = p, xout = 0.0, rule = 2L)$y
    )

    # Test null hypothesis and compute 0-value.
    if (object@alternative == "two_sided") {
      reject_null <- ci[1L] >= 0.0 || (ci[2L] <= 0.0)
      p_value <- min(c(p_value, 1.0 - p_value)) * 2.0

    } else if (object@alternative == "less") {
      reject_null <- ci[1L] <= 0.0
      ci <- c(-Inf, ci[1L])
      p_value <- 1.0 - p_value

    } else if (object@alternative == "greater") {
      reject_null <- ci[1L] >= 0.0
      ci <- c(ci[1L], Inf)
      p_value <- p_value

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }

    return(list(
      "effect" = effect,
      "confidence_interval" = ci,
      "p_value" = p_value,
      "reject_null" = reject_null,
      "alternative" = object@alternative
    ))
  }
)


## .compute_test_results --------------------------------------------------------
setMethod(
  ".compute_test_results",
  signature(object = "empiricalDiffTest"),
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



# empiricalPairedDiffTest object -----------------------------------------------
setClass(
  "empiricalPairedDiffTest",
  contains = "empiricalDiffTest",
  prototype = list(
    name =  "paired (two-sample) bootstrap difference test",
    requires_summary_function = FALSE
  )
)



## initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalPairedDiffTest"),
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

    return(.Object)
  }
)


## get_n_draw_input_samples ----------------------------------------------------
setMethod(
  "get_n_draw_input_samples",
  signature(
    object = "empiricalPairedDiffTest"
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
  signature(object = "empiricalPairedDiffTest"),
  function(object) {
    if (is.null(object@summary_data)) return(0L)

    # The same samples should be drawn for each input.
    return(length(object@summary_data[[1L]]))
  }
)


## .sample_data ----------------------------------------------------------------
setMethod(
  ".sample_data",
  signature(object = "empiricalPairedDiffTest"),
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
