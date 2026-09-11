#' @include generic_test.R
#' @include generic_location_test.R
NULL

#' Title
#'
#' @param ... description
#' @param alternative description
#' @param significance_level description
#' @param summary_function description
#' @param summary_function_args description
#'
#' @returns htest object
#' @export
difference_test <- function(
    ...,
    alternative = "two_sided",
    significance_level = 0.95,
    summary_function = mean,
    summary_function_args = NULL
) {
  # Create test object.
  object <- methods::new(
    "empiricalDiffTest",
    data = list(...),
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




# empiricalDiffTest object -----------------------------------------------------
setClass(
  "empiricalDiffTest",
  contains = "empiricalGenericLocTest",
)


## initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalDiffTest"),
  function(
    .Object,
    ...
  ) {
    .Object@name <- "empirical difference test"

    # Update with parent class first. This checks significance_level.
    .Object <- callNextMethod()

    # Check specific conditions for data.
    .check_input_data(
      .Object@input_data,
      condition = c("at_least", "2")
    )

    # Check that a summary function is set (unpaired empirical difference tests
    # cannot operate on raw data).
    if (is.null(.Object@summary_function)) {
      rlang::abort(
        message = paste0(
          "The summary function cannot be empty for ", .Object@name, ". "
        )
      )
    }

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

    # Replace name.
    .Object@name <- "empirical paired difference test"

    # Check that all data entries have the same length.
    browser()
    rlang::abort("Implement this check")

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

    # The default is for the same samples to be randomly selected.
    n_samples <- length(data[[1L]])
    sample_indices <- as.integer(ceiling(random_values[1L:n_samples] * n_samples))

    # These edge cases are highlu unlikely but not impossible.
    sample_indices[sample_indices < 1L] <- 1L
    sample_indices[sample_indices > n_samples] <- n_samples

    sampled_data <- list()
    for (variable in names(data)) {
      # Select samples.
      sampled_data[[variable]] <- data[[variable]][sample_indices]
    }

    return(sampled_data)
  }
)
