#' @include generic_test.R
#' @include generic_goodness_of_fit_test.R
NULL

# empiricalMultinomialGOFTest object -----------------------------------------------
setClass(
  "empiricalMultinomialGOFTest",
  contains = "empiricalGenericGOFTest",
  prototype = list(
    percentiles = NA_real_,
    expected = NA_real_,
    test_statistic = NULL,
    test_statistic_args = NULL
  )
)


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalGenericGOFTest"),
  function(
    .Object,
    data,
    expected,
    test_statistic,
    test_statistic_args,
    ...
  ) {
    # Pass to empiricalTest for global attributes.
    .Object <- callNextMethod()

    if (rlang::is_missing(data)) ..error_missing_variable("data")
    if (rlang::is_missing(expected)) ..error_missing_variable("expected")
    if (rlang::is_missing(test_statistic)) ..error_missing_variable("test_statistic")
    if (rlang::is_missing(test_statistic_args)) ..error_missing_variable("test_statistic_args")

    # Format data.
    .Object@input_data <- format_data_categorical(data = data)

    return(.Object)
  }
)
