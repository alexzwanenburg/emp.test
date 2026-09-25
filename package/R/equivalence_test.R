#' @include generic_test.R
#' @include diff_test.R
NULL

#' Title
#'
#' @param ... description; the first variable is used as control. The computed
#'   difference are between test and control groups (test - control).
#' @param tolerance description
#' @param alternative description one of "equivalent", "not_inferior" or "not_superior".
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
    alternative = "equivalent",
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
      tolerance = tolerance,
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
      tolerance = tolerance,
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



# empiricalEquivalenceTest object ----------------------------------------------
setClass(
  "empiricalEquivalenceTest",
  contains = "empiricalDiffTest",
  slots = list(
    tolerance = "ANY",
    tolerance_function_args = "ANY"
  ),
  prototype = list(
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
          list(.Object@input_data[[1L]][1L], .Object@input_data[[2L]][1L]),
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
        x = y,
        var_name = "tolerance",
        min = 1L,
        max = 2L
      )

      .Object@tolerance <- tolerance

    } else {
      rlang::abort(
        message = "tolerance must be one or two (lower, upper tolerance) numeric values or a function that yields such values",
        class = c("emp_test_error", "input_argument_error")
      )
    }

    return(.Object)
  }
)



## ..test -----------------------------------------------------------------------
setMethod(
  "..test",
  signature(object = "empiricalEquivalenceTest"),
  function(
    object,
    x_1,
    x_2,
    hypothesis_test_only = FALSE,
    ...
  ) {
    # Compute or set tolerance values.
    if (is.numeric(object@tolerance)) {
      tolerance <- object@tolerance
      if (length(tolerance) > 1L) {
        # Asymmetric tolerance bounds
        tolerance_lower <- rep.int(tolerance[1L], length(x_1))
        tolerance_upper <- rep.int(tolerance[2L], length(x_1))

      } else {
        # Symmetric tolerance bounds.
        tolerance_lower <- rep.int(-tolerance[1L], length(x_1))
        tolerance_upper <- rep.int(tolerance[1L], length(x_1))
      }

    } else if (rlang::is_function(object@tolerance)) {
      # Compute tolerance based on
      tolerance <- mapply(
        object@tolerance,
        x_1,
        x_2,
        MoreArgs = object@tolerance_function_args,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
      if (length(tolerance[[1L]]) > 1L) {
        tolerance_lower <- sapply(tolerance, function(x) (x[1L]))
        tolerance_upper <- sapply(tolerance, function(x) (x[2L]))
      } else {
        tolerance_upper <- sapply(tolerance, function(x) (x[1L]))
        tolerance_lower <- -tolerance_upper
      }
    }

    # Count the number of times we observe that observations according to the
    # null-hypothesis (the difference is outside of tolerance) are observed.
    if (object@alternative == "equivalent") {
      p <- (x_2 - x_1 <= tolerance_lower || x_2 - x_1 > tolerance_upper)

    } else if (object@alternative == "not_inferior") {
      p <- (x_2 - x_1 <= tolerance_lower)

    } else if (object@alternative == "not_superior") {
      p <- (x_2 - x_1 >= tolerance_upper)

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }
    p_value <- sum(p) / length(p)
    reject_null <- p_value <= 1.0 - object@alternative

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
