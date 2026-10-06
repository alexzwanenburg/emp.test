#' @include generic_test.R
#' @include bootstrap_location_test.R


# empiricalEquivalenceTest object ----------------------------------------------
setClass(
  "empiricalEquivalenceLocTest",
  contains = "empiricalLocTest",
  slots = list(
    tolerance = "ANY",
    tolerance_function_args = "ANY"
  ),
  prototype = list(
    # name is set within the initialize method (depends on alternative)
    alternative_values = c("equivalent", "not_inferior", "not_superior"),
    tolerance = NULL,
    tolerance_function_args = NULL
  )
)


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalEquivalenceLocTest"),
  function(
    .Object,
    tolerance,
    tolerance_function_args = NULL,
    ...
  ) {
    # Pass to empiricalTest for global attributes.
    .Object <- callNextMethod()

    # Set tolerance and function arguments, if any.
    .check_tolerance(
      object = .Object,
      tolerance = tolerance,
      tolerance_function_args = tolerance_function_args,
      x_1 = .Object@input_data[[1L]][1L]
    )
    .Object@tolerance <- tolerance
    .Object@tolerance_function_args <- tolerance_function_args

    # Set name
    .Object@name <- switch(
      .Object@alternative,
      "equivalent" = "bootstrap (one-sample) equivalence test",
      "not_inferior" = "bootstrap (one-sample) non-inferiority test",
      "not_superior" = "bootstrap (one-sample) non-superiority test"
    )

    return(.Object)
  }
)



## .set_percentiles ------------------------------------------------------------
setMethod(
  ".set_percentiles",
  signature(object = "empiricalEquivalenceLocTest"),
  function(
    object,
    ...
  ) {
    if (object@alternative == "equivalent") {
      # group 1 has either smaller or larger summary statistic than group 2
      # by default (0.05, 0.95)
      pctl <- c(1.0 - object@significance_level, object@significance_level)

    } else if (object@alternative == "not_inferior") {
      # by default (0.05)
      pctl <- 1.0 - object@significance_level

    } else if (object@alternative == "not_superior") {
      # by default (0.95)
      pctl <- object@significance_level

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }

    object@percentiles <- pctl

    return(object)
  }
)



## ..test -----------------------------------------------------------------------
setMethod(
  "..test",
  signature(object = "empiricalEquivalenceLocTest"),
  function(
    object,
    x_1,
    hypothesis_test_only = FALSE,
    ...
  ) {
    # Get tolerance bounds
    tolerance <- .get_tolerance_bounds(
      object = object,
      x_1 = x_1
    )
    tolerance_lower <- tolerance$tolerance_lower
    tolerance_upper <- tolerance$tolerance_upper

    return(..test_equivalence(
      object = object,
      x = x_1,
      hypothesis_test_only = hypothesis_test_only,
      tolerance_lower = tolerance_lower,
      tolerance_upper = tolerance_upper
    ))
  }
)
