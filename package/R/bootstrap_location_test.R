#' @include generic_test.R
#' @include generic_location_test.R
NULL

#' Bootstrap location test
#'
#' Performs a one-sample location test on variables.
#'
#' @param ... One or more numeric variables that are to be tested. If more than
#'   one numeric variable is provided, tests are conducted separately for each
#'   variable. In this case it is recommended to set symmetric `tolerance`
#'   boundaries or generate this dynamically.
#' @param location Numeric value against which numeric variables are tested.
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
#' @details A location test assesses the difference between a variable (sample)
#'   and a location. An example of a difference test is Student's one-sample
#'   t-test, which assesses the between the mean of a variable and the location.
#'   The bootstrap location test generalises such tests by allowing a custom
#'   `summary_function`, and not having any assumptions except for independence.
#'
#'   The difference test has the null hypothesis \eqn{H_0: f(x) = \lambda}{H0:
#'   f(x) = λ}. Here \eqn{f(x)}{f(x)} is the summary descriptor of input variable \eqn{x}{x},
#'   and \eqn{\lambda}{λ} is the location..
#'
#'   The alternative hypothesis depends on `alternative`.
#'   When `alternative = "two_sided"`, the alternative hypothesis is
#'   \eqn{H_1: f(x) \neq λ}{H1: f(x) ≠ λ}.
#'   For `alternative = "greater"` the alternative hypothesis is
#'   \eqn{H_1: f(x) > λ}{H1: f(x) > λ}.
#'   Finally, for `alternative = "less"` the alternative hypothesis is
#'    \eqn{H_1: f(x) < λ}{H1: f(x) < λ}.
#'
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
    name =  "bootstrap (one-sample) location test",
    location = NA_real_,
    requires_summary_function = FALSE
  )
)


## initialize ------------------------------------------------------------------
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

    # Set n_draw_input_samples, which determines the number of random numbers
    # that should be drawn for summaries,
    .Object@n_draw_input_samples <- get_n_draw_input_samples(.Object)

    return(.Object)
  }
)



## .test -----------------------------------------------------------------------
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


## ..test ----------------------------------------------------------------------
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
    # and p-value computation can work with sorted data.
    x <- sort(x_1)

    return(..test_difference(
      object = object,
      x = x,
      hypothesis_test_only = hypothesis_test_only,
      location = object@location
    ))
  }
)



## .compute_test_results -------------------------------------------------------
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
