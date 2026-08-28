#' @include generic_test.R
NULL

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
    alternative = "alternative",
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
  contains = "empiricalTest",
  slots = list(
    alternative = "character",
    summary_function = "ANY",
    summary_function_args = "ANY"
  ),
  prototype = list(
    name = "empirical difference test",
    alternative = "two.sided",
    summary_function = NULL,
    summary_function_args = NULL
  )
)


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalDiffTest"),
  function(
    .Object,
    data,
    significance_level,
    alternative,
    summary_function,
    summary_function_args,
    ...
  ) {
    # Update with parent class first. This checks significance_level.
    .Object <- callNextMethod()

    data <- format_data(
      data = data,
      condition = c("at_least", "2")
    )

    # Set the alternative hypothesis.
    .check_parameter_value_is_valid(
      x = alternative,
      var_name = "alternative",
      values = c("two.sided", "two_sided", "greater", "less")
    )
    if (alternative == "two.sided") alternative <- "two-sided"
    .Object@alternative <- alternative

    # Check summary_function.
    # 1. That the function is a function.
    .check_is_function(
      x = summary_function,
      var_name = "summary_function"
    )

    # 2. That the function returns a single numeric value.
    y <- do.call(
      summary_function,
      args = c(
        list(data[[1L]]),
        summary_function_args
      )
    )
    .check_is_numeric(
      x = y,
      var_name = "summary_function output"
    )
    .check_argument_length(
      x = y,
      var_name = "summary_function output",
      min = 1L,
      max = 1L
    )
    .Object@summary_function <- summary_function
    .Object@summary_function_args <- summary_function_args

    # Set n_draw_samples, which determines the number of random numbers that
    # should be drawn.
    .Object@n_draw_samples <- get_n_draw_samples(.Object)

    return(.Object)
  }
)


