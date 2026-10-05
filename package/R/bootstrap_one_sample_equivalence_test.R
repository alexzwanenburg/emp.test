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

    # Count the number of times we observe that observations according to the
    # null-hypothesis (the difference is outside of tolerance) are observed.
    if (object@alternative == "equivalent") {
      # For consistency with the two one-sided test (TOST) procedure, the
      # boundaries are tested separately.
      p_lower <- x_1 <= tolerance_lower
      p_upper <- x_1 >= tolerance_upper
      p_value <- max(sum(p_lower), sum(p_upper)) / length(p_lower)

    } else if (object@alternative == "not_inferior") {
      p <- (x_1 <= tolerance_lower)
      p_value <- sum(p) / length(p)

    } else if (object@alternative == "not_superior") {
      p <- (x_1 >= tolerance_upper)
      p_value <- sum(p) / length(p)

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }

    reject_null <- p_value <= 1.0 - object@significance_level

    if (hypothesis_test_only) {
      return(list("reject_null" = reject_null))
    }

    # Compute effect.
    effect <- stats::median(x_1, type = 8L)

    return(list(
      "effect" = effect,
      "p_value" = p_value,
      "reject_null" = reject_null,
      "alternative" = object@alternative
    ))
  }
)
