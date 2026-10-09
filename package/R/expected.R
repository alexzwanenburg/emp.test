.set_expected_categorical <- function(expected, data) {
  # Null -----------------------------------------------------------------------
  # In this case, the null hypothesis-data is generated from the combined
  # counts of the input data.
  if (is.null(expected)) {
    expected <- data[[1L]]
    if (length(data) > 1L) {
      for (ii in 2L:length(data)) {
        expected <- expected + data[[ii]]
      }
    }

    # If there are any 0s, insert half counts.
    expected[expected == 0L] <- 0.5

    # Normalise to 1.
    expected <- expected / sum(expected)
    return(expected)
  }

  # integer --------------------------------------------------------------------
  # Expected are counts.
  if (is.integer(expected)) {

    # Check that the number of categories is the same as the input data.
    if (length(data[[1L]]) != length(expected)) {
      rlang::abort(
        message = paste0(
          "Integer values of expected are expected to represent expected counts of the categories. ",
          "Found: different number of categories: expected (", length(expected), "), data (", length(data[[1L]]), ")."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    # Check that there are no negative entries.
    if (any(expected < 0L)) {
      rlang::abort(
        message = paste0(
          "Integer values of expected are expected to represent expected counts of the categories. ",
          "Found: one or more negative values."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    # If expected has names, check that these match the provided
    if (!is.null(names(expected))) {
      if (!identical(names(expected), names(data[[1L]]))) {
        rlang::abort(
          message = paste0(
            "Integer values of expected are expected to represent expected counts of the categories. ",
            "Found: mismatch in categories between expected and data."
          ),
          class = c("emp_test_error", "input_argument_error")
        )
      }
    }

    # Normalise to 1. We don't add 0.5 for counts with 0.
    expected <- expected / sum(expected)
    return(expected)
  }

  # numeric --------------------------------------------------------------------
  # Expected are frequencies.
  if (is.numeric(expected)) {
    # Check that the number of categories is the same as the input data.
    if (length(data[[1L]]) != length(expected)) {
      rlang::abort(
        message = paste0(
          "Numeric values of expected are expected to represent probabilities of the categories. ",
          "Found: different number of categories: expected (", length(expected), "), data (", length(data[[1L]]), ")."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    # Check that there are no negative entries.
    if (any(expected < 0L)) {
      rlang::abort(
        message = paste0(
          "Numeric values of expected are expected to represent probabilities of the categories. ",
          "Found: one or more negative values."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    # If expected has names, check that these match the provided
    if (!is.null(names(expected))) {
      if (!identical(names(expected), names(data[[1L]]))) {
        rlang::abort(
          message = paste0(
            "Numeric values of expected are expected to represent probabilities of the categories. ",
            "Found: mismatch in categories between expected and data."
          ),
          class = c("emp_test_error", "input_argument_error")
        )
      }
    }

    # Check if expected sums to 1.
    if (!all.equal(sum(expected), 1.0)) {
      rlang::warn(
        message = paste0(
          "Numeric values of expected are expected to represent probabilities of the categories. ",
          "Found: expected does not sum to 1.0 (", sum(expected), "). Values will be normalised."
        ),
        class = c("emp_test_warning", "input_argument_warning")
      )
    }
    expected <- expected / sum(expected)

    return(expected)
  }


  # logical --------------------------------------------------------------------
  if (is.logical(expected)) {
    categories <- names(data[[1L]])
    if (!all(expected %in% categories)) {
      rlang::abort(

      )
    }
  }

  # character ------------------------------------------------------------------

  # factor ---------------------------------------------------------------------

  # function -------------------------------------------------------------------
  # Expected is a function that can randomly generate values.

}
