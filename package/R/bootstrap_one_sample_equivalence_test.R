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

    if (rlang::is_missing(tolerance)) ..error_missing_variable("tolerance")

    # tolerance can be either a function or one or two numeric values (lower,
    # upper tolerance).
    if (rlang::is_function(tolerance)) {
      # Check that the function returns one or two values.
      y <- do.call(
        tolerance,
        args = c(
          list(.Object@input_data[[1L]][1L]),
          tolerance_function_args
        )
      )

      .check_is_numeric(
        x = y,
        var_name = "tolerance output"
      )
      .check_argument_length(
        x = y,
        var_name = "tolerance output",
        min = 1L,
        max = 2L
      )

      .Object@tolerance <- tolerance
      .Object@tolerance_function_args <- tolerance_function_args

    } else if (is.numeric(tolerance)) {
      # Check that tolerance consists of one or two numeric values.
      .check_argument_length(
        x = tolerance,
        var_name = "tolerance",
        min = 1L,
        max = 2L
      )

      .Object@tolerance <- tolerance

    } else {
      msg <- "tolerance must be one "
      if (.Object@alternative == "equivalent") {
        msg <- c(msg, "or two (lower, upper tolerance) numeric values ")
      } else if (.Object@alternative == "not_inferior") {
        msg <- c(msg, "(lower tolerance) numeric value ")
      } else if (.Object@alternative == "not_superior") {
        msg <- c(msg, "(upper tolerance) numeric value ")
      }
      msg <- c(msg, "or a function that yields such values.", collapse = "")

      rlang::abort(
        message = paste0(msg),
        class = c("emp_test_error", "input_argument_error")
      )
    }

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
      p_lower <- x_2 - x_1 <= tolerance_lower
      p_upper <- x_2 - x_1 >= tolerance_upper
      p_value <- max(sum(p_lower), sum(p_upper)) / length(p_lower)

    } else if (object@alternative == "not_inferior") {
      p <- (x_2 - x_1 <= tolerance_lower)
      p_value <- sum(p) / length(p)

    } else if (object@alternative == "not_superior") {
      p <- (x_2 - x_1 >= tolerance_upper)
      p_value <- sum(p) / length(p)

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }

    reject_null <- p_value <= 1.0 - object@significance_level

    if (hypothesis_test_only) {
      return(list("reject_null" = reject_null))
    }

    # Compute effect.
    effect <- stats::median(x_2 - x_1, type = 8L)

    return(list(
      "effect" = effect,
      "p_value" = p_value,
      "reject_null" = reject_null,
      "alternative" = object@alternative
    ))
  }
)
