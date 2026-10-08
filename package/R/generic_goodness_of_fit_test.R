#' @include generic_test.R
NULL


# empiricalGenericLocTest object -----------------------------------------------
setClass(
  "empiricalGenericGOFTest",
  contains = "empiricalTest",
  slots = list(
    percentiles = "numeric",
    expected = "ANY",
    test_statistic = "ANY",
    test_statistic_args = "ANY"
  ),
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
    ...
  ) {
    # Pass to empiricalTest for global attributes.
    .Object <- callNextMethod()

    if (rlang::is_missing(data)) ..error_missing_variable("data")
    if (rlang::is_missing(expected)) ..error_missing_variable("expected")
    if (rlang::is_missing(test_statistic)) ..error_missing_variable("test_statistic")
    if (rlang::is_missing(test_statistic_args)) ..error_missing_variable("test_statistic_args")

    return(.Object)
  }
)

# Bootstrap goodness of fit steps:

# Test-statistic route:
# Compute test statistic on input data.
# Draw samples from the expected distribution (either external or ad-hoc).
# Compute test statistics for each sample.
# Compare test input and bootstrap test statistics.
#
# Bootstrap route:
# Sort input data.
# Draw samples from the expected distribution (either external or ad-hoc)
# Sort samples.
# Compare each input datapoint with its corresponding expected distribution.
