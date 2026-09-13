#' @include generic_test.R
#' @include generic_location_test.R
NULL

#' Title
#'
#' @param ... description
#' @param tolerance description
#' @param alternative description one of "not_equivalent", "inferior" or "superior".
#' @param significance_level description
#' @param paired description
#' @param summary_function description
#' @param summary_function_args description
#'
#' @returns htest object
#' @export
equivalence_test <- function(
    ...,
    tolerance,
    alternative = "not_equivalent",
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
