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
emp_diff_test <- function(
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
}




# empiricalDiffTest object -----------------------------------------------------
setClass(
  "empiricalDiffTest",
  contains = "empiricalGenericLocTest",
)


# initialize -------------------------------------------------------------------
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


setMethod(
  "..bootstrap_test",
  signature(object = "empiricalDiffTest"),
  function(
    object,
    ...
  ) {
    # Produce samples.
    samples <- .sample_data(object, ...)

    # Iterate over combinations of groups.

    # Per group determine
  }
)
