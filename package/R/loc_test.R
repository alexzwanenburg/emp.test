#' @include generic_test.R
#' @include generic_location_test.R
NULL

#' Bootstrap location test
#'
#' @param ... description
#' @param location description
#' @param alternative description
#' @param significance_level description
#' @param summary_function description
#' @param summary_function_args description
#'
#' @returns htest object
#' @export
location_test <- function(
    ...,
    location = location,
    alternative = "two_sided",
    significance_level = 0.95,
    summary_function = mean,
    summary_function_args = NULL
) {
  # Create test object.
  object <- methods::new(
    "empiricalLocTest",
    data = list(...),
    location = location,
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




# empiricalLocTest object -----------------------------------------------------
setClass(
  "empiricalLocTest",
  contains = "empiricalGenericLocTest",
  slots = list(
    location = "numeric"
  ),
  prototype = list(
    name =  "empirical location test",
    location = NA_real_,
    requires_summary_function = FALSE
  )
)


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalLocTest"),
  function(
    .Object,
    location,
    ...
  ) {
    if (rlang::is_missing(location)) ..error_missing_variable("location")

    # Update with parent class first. This checks significance_level.
    .Object <- callNextMethod()

    # Check specific conditions for data.
    .check_input_data(
      .Object@input_data,
      condition = c("at_least", "1")
    )

    # Check location.
    .check_is_numeric(
      x = location,
      var_name = "location"
    )
    .Object@location <- location

    # Set n_draw_input_samples, which determines the number of random numbers that
    # should be drawn for summaries,
    .Object@n_draw_input_samples <- get_n_draw_input_samples(.Object)

    return(.Object)
  }
)



# .test ------------------------------------------------------------------------
setMethod(
  ".test",
  signature(object = "empiricalLocTest"),
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


# ..test -----------------------------------------------------------------------
setMethod(
  "..test",
  signature(object = "empiricalLocTest"),
  function(
    object,
    x_1,
    hypothesis_test_only = FALSE,
    ...
  ) {
    # Compute difference between groups. Pre-sort so that both quantile
    # and
    x <- sort(x_1)

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
        reject_null <- ci[1L] >= object@location || (ci[2L] <= object@location)

      } else if (object@alternative == "less") {
        reject_null <- ci[1L] <= object@location

      } else if (object@alternative == "greater") {
        reject_null <- ci[1L] >= object@location

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
      stats::approx(x = x, y = p, xout = object@location, rule = 2L)$y
    )

    # Test null hypothesis and compute 0-value.
    if (object@alternative == "two_sided") {
      reject_null <- ci[1L] >= object@location || (ci[2L] <= object@location)
      p_value <- min(c(p_value, 1.0 - p_value)) * 2.0

    } else if (object@alternative == "less") {
      reject_null <- ci[1L] <= object@location
      ci <- c(-Inf, ci[1L])
      p_value <- 1.0 - p_value

    } else if (object@alternative == "greater") {
      reject_null <- ci[1L] >= object@location
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
      "alternative" = object@alternative,
      "location" = object@location
    ))
  }
)


# .compute_test_results --------------------------------------------------------
setMethod(
  ".compute_test_results",
  signature(object = "empiricalLocTest"),
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
