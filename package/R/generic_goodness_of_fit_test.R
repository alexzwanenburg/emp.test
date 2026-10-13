#' @include generic_test.R
NULL


# empiricalGenericLocTest object -----------------------------------------------
setClass(
  "empiricalGenericGOFTest",
  contains = "empiricalTest",
  slots = list(
    percentiles = "numeric",
    expected = "ANY"
  ),
  prototype = list(
    percentiles = NA_real_,
    expected = NA_real_
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

    return(.Object)
  }
)


# .compute_test_statistic ------------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric(".compute_test_statistic", function(object, ...) standardGeneric(".compute_test_statistic"))


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
