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
  signature(.Object = "empiricalEquivalenceTest"),
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
      "equivalent" = "bootstrap (two-sample) equivalence test",
      "not_inferior" = "bootstrap (two-sample) non-inferiority test",
      "not_superior" = "bootstrap (two-sample) non-superiority test"
    )

    return(.Object)
  }
)
