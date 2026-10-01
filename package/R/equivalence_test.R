#' @include generic_test.R
#' @include diff_test.R
NULL

#' Bootstrap equivalence test
#'
#' @param ... Two or more numeric variables that are to be tested. If exactly
#'   two variables are provided, the first variable is used as the control
#'   group. The computed difference are between test and control groups (test -
#'   control). If `paired = FALSE` (default), the number of values in each
#'   variable can be different. If `paired = TRUE`, the number of values in each
#'   variable must be the same.
#'
#'   If more than two numeric variables are provided, tests are conducted in
#'   pairwise combinations. In this case it is recommended to set symmetric
#'   `tolerance` boundaries or generate this dynamically.
#' @param tolerance Numeric values setting the lower and / or upper boundary for
#'   the test. If `alternative = "equivalent`, one or two values are expected.
#'   If one value is provided, the provided value is used as the upper boundary,
#'   and its negative counterpart is used as the lower boundary, that is, the
#'   boundaries are symmetric around 0. If `alternative = "not_inferior"`, one
#'   value is expected, which is used as the lower boundary. If `alternative =
#'   "not_superior"`, one value is expected, which is used as the upper
#'   boundary.
#'
#'   Instead of providing numeric values, a custom function can be provided that
#'   yields one or two numeric values (depending on `alternative`). The
#'   **first two** arguments of the function are expected to be single numeric values
#'   that were computed after applying `summary_function` to the a pair of
#'   columns of the input data. This allows for dynamically computing tolerance
#'   values based on summary statistic values.
#' @param tolerance_function_args A named list of additional arguments supplied
#'   to `tolerance`, if `tolerance` is a custom function.
#' @param alternative The alternative hypothesis. One of `"equivalent"` (default),
#'   `"not_inferior"` or `"not_superior"`.
#' @inheritParams difference_test
#'
#' @returns htest object
#' @export
equivalence_test <- function(
    ...,
    tolerance,
    tolerance_function_args = NULL,
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
      tolerance_function_args = tolerance_function_args,
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
      tolerance_function_args = tolerance_function_args,
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
        x = tolerance,
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


## .set_percentiles ------------------------------------------------------------
setMethod(
  ".set_percentiles",
  signature(object = "empiricalEquivalenceTest"),
  function(
    object,
    ...
  ) {
    # For equivalence tests
    return(object)
  }
)



## .test -----------------------------------------------------------------------
setMethod(
  ".test",
  signature(object = "empiricalEquivalenceTest"),
  function(
    object,
    data,
    hypothesis_test_only = FALSE,
    ...
  ) {
    test_output <- list()

    # Iterate over combinations of groups.
    for (groups in combn(names(data), m = 2L, simplify = FALSE)) {
      results <- ..test(
        object = object,
        x_1 = data[[groups[1L]]],
        x_2 = data[[groups[2L]]],
        hypothesis_test_only = hypothesis_test_only
      )

      # Add control and test group.
      if (!hypothesis_test_only) {
        results$control_group <- groups[1L]
        results$test_group <- groups[2L]
      }

      test_output[[paste0(groups[2L], " - ", paste0(groups[1L]))]] <- results
    }

    return(test_output)
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
        if (object@alternative == "equivalent") {
          # Symmetric tolerance bounds.
          tolerance_lower <- rep.int(-tolerance[1L], length(x_1))
          tolerance_upper <- rep.int(tolerance[1L], length(x_1))

        } else if (object@alternative == "not_inferior") {
          tolerance_lower <- rep.int(tolerance[1L], length(x_1))

        } else if (object@alternative == "not_superior") {
          tolerance_upper <- rep.int(tolerance[1L], length(x_1))

        } else {
          ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
        }
      }

    } else if (rlang::is_function(object@tolerance)) {
      # Compute tolerance based on a user-provided function.
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
