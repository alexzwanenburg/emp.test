.set_expected_categorical <- function(
    expected,
    expected_args = NULL,
    data
) {
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
    if (any(expected < 0.0)) {
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


  # logical or character -------------------------------------------------------
  if (is.logical(expected) || is.character(expected)) {
    categories <- names(data[[1L]])
    if (!all(expected %in% categories)) {
      rlang::abort(
        message = paste0(
          "Expected and data are expected to have the same categories. Found: ",
          paste_s(setdiff(unique(expected), categories)), " appear in expected but not in the data."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }
    counts <- integer(length(categories))
    for (jj in seq_along(categories)) {
      counts[jj] <- sum(expected == categories[jj])
    }
    names(counts) <- categories
    expected <- counts / sum(counts)

    return(expected)
  }


  # factor ------------------------------------------------------------------
  if (is.character(expected)) {
    categories <- names(data[[1L]])
    if (!all(levels(expected) %in% categories)) {
      rlang::abort(
        message = paste0(
          "Expected and data are expected to have the same categories. Found: ",
          paste_s(setdiff(levels(expected), categories)), " appear in expected but not in the data."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }
    counts <- integer(length(categories))
    for (jj in seq_along(categories)) {
      counts[jj] <- sum(expected == categories[jj])
    }
    names(counts) <- categories
    expected <- counts / sum(counts)

    return(expected)
  }


  # function -------------------------------------------------------------------
  if (is.function(expected)) {
    # Attempt to generate values. We generate 1000 values so that we are likely
    # to capture most of the output.
    x <- do.call(
      expected,
      args = c(
        list("n" = 1000L),
        expected_args
      )
    )

    # The output should either be integer values indicating specific categories
    # or the categories themselves.
    categories <- names(data[[1L]])
    if (is.integer(x)) {
      if (any(x <= 0L) || any(x > length(categories))) {
        rlang::abort(
          message = paste0(
            "The output of expected and the provided data are expected to match, ",
            "with integer values directly associated to each category. Found: ",
            "output of value 0 or lower, or output exceeding the number of categories ",
            "(", length(categories), ")."
          ),
          class = c("emp_test_error", "input_argument_error")
        )
      }

    } else {
      if (!all(x %in% categories)) {
        rlang::abort(
          message = paste0(
            "The output of expected and the provided data are expected to have the same categories. Found: ",
            paste_s(setdiff(unique(x), categories)), " appear as output of expected but not in the data."
          ),
          class = c("emp_test_error", "input_argument_error")
        )
      }
    }

    return(expected)
  }

  # If we get here, expected has an unexpected type.
  rlang::abort(
    message = paste0(
      "expected did not have a valid type: ", paste_s(class(expected)), ". Please consult the documentation."
    ),
    class = c("emp_test_error", "input_argument_error")
  )

  return(invisible(NULL))
}
