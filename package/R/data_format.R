format_data <- function(data) {
  # This converts any input data to a list of numeric vectors.
  formatted_data <- list()
  formatted_data_names <- character()
  data_index <- 1L

  for (ii in seq_along(data)) {
    data_set <- data[[ii]]
    var_name <- names(data)[ii]
    if (is.data.frame(data_set)) {
      # Covers data.frame and data.table.
      for (column in colnames(data_set)) {
        if (is.numeric(data_set[[column]])) {
          formatted_data[[data_index]] <- data_set[[column]]
          formatted_data_names[data_index] <- column
          data_index <- data_index + 1L
        }
      }

    } else if (rlang::is_bare_list(data_set)) {
      # Covers pure lists.
      for (ii in seq_along(data_set)) {
        if (is.numeric(data_set[[ii]])) {
          formatted_data[[data_index]] <- data_set[[ii]]
          if (!is.null(names(data_set)[ii])) {
            formatted_data_names[data_index] <- names(data_set)[ii]
          } else {
            formatted_data_names[data_index] <- paste0("x_", ii)
          }
          data_index <- data_index + 1L
        }
      }

    } else if (is.numeric(data_set)) {
      # Numeric vectors.
      formatted_data[[data_index]] <- data_set
      if (!is.null(var_name)) {
        formatted_data_names[data_index] <- var_name
      } else {
        formatted_data_names[data_index] <- paste0("x_", ii)
      }
      data_index <- data_index + 1L

    } else {
      msg <- "While attempting to convert the data, a variable that does not contain numeric data was detected: "
      if (is.null(var_name)) {
        msg <- paste0(msg, "positional variable [", ii, "]")
      } else {
        msg <- paste0(msg, var_name)
      }

      rlang::abort(
        message = msg,
        class = c("emp_test_error", "input_argument_error")
      )
    }
  }

  # Basic checks
  # Check that any data is present.
  if (length(formatted_data) == 0L) {
    rlang::abort(
      message = "No numeric data was found.",
      class = c("emp_test_error", "input_argument_error")
    )
  }

  # Check that none of the data has zero length.
  if (any(lengths(formatted_data) == 0L)) {
    variables_empty <- formatted_data_names[lengths(formatted_data)]
    rlang::abort(
      message = paste0(
        "One or more empty variables were detected: ", paste_s(variables_empty)
      ),
      class = c("emp_test_error", "input_argument_error")
    )
  }

  # Set names.
  names(formatted_data) <- formatted_data_names

  return(formatted_data)
}



format_data_categorical <- function(data) {
  # This converts any input data to a list of numeric vectors.
  formatted_data <- list()
  formatted_data_names <- character()
  data_index <- 1L

  for (ii in seq_along(data)) {
    data_set <- data[[ii]]
    var_name <- names(data)[ii]
    if (is.data.frame(data_set)) {
      # Covers data.frame and data.table.
      for (column in colnames(data_set)) {
        if (
          is.character(data_set[[column]]) || is.logical(data_set[[column]]) ||
          is.integer(data_set[[column]]) || is.factor(data_set[[column]])
        ) {
          formatted_data[[data_index]] <- data_set[[column]]
          formatted_data_names[data_index] <- column
          data_index <- data_index + 1L
        }
      }

    } else if (rlang::is_bare_list(data_set)) {
      # Covers pure lists.
      for (ii in seq_along(data_set)) {
        if (
          is.character(data_set[[ii]]) || is.logical(data_set[[ii]]) ||
          is.integer(data_set[[ii]]) || is.factor(data_set[[ii]])
        ) {
          formatted_data[[data_index]] <- data_set[[ii]]

          if (!is.null(names(data_set)[ii])) {
            formatted_data_names[data_index] <- names(data_set)[ii]
          } else {
            formatted_data_names[data_index] <- paste0("x_", ii)
          }
          data_index <- data_index + 1L
        }
      }

    } else if (
      is.character(data_set) || is.logical(data_set) ||
      is.integer(data_set) || is.factor(data_set)
    ) {
      # Numeric vectors.
      formatted_data[[data_index]] <- data_set
      if (!is.null(var_name)) {
        formatted_data_names[data_index] <- var_name
      } else {
        formatted_data_names[data_index] <- paste0("x_", ii)
      }
      data_index <- data_index + 1L

    } else {
      msg <- "While attempting to convert the data, a variable that does not contain numeric data was detected: "
      if (is.null(var_name)) {
        msg <- paste0(msg, "positional variable [", ii, "]")
      } else {
        msg <- paste0(msg, var_name)
      }

      rlang::abort(
        message = msg,
        class = c("emp_test_error", "input_argument_error")
      )
    }
  }

  # Basic checks
  # Check that any data is present.
  if (length(formatted_data) == 0L) {
    rlang::abort(
      message = "No categorical data was found.",
      class = c("emp_test_error", "input_argument_error")
    )
  }

  # Check that none of the data has zero length.
  if (any(lengths(formatted_data) == 0L)) {
    variables_empty <- formatted_data_names[lengths(formatted_data)]
    rlang::abort(
      message = paste0(
        "One or more empty variables were detected: ", paste_s(variables_empty)
      ),
      class = c("emp_test_error", "input_argument_error")
    )
  }

  # Check that the class of all columns is the same.
  all_data_same_class <- FALSE
  if (is.character(formatted_data[[1L]])) all_data_same_class <- all(sapply(formatted_data, is.character))
  else if (is.logical(formatted_data[[1L]])) all_data_same_class <- all(sapply(formatted_data, is.logical))
  else if (is.integer(formatted_data[[1L]])) all_data_same_class <- all(sapply(formatted_data, is.integer))
  else if (is.factor(formatted_data[[1L]])) all_data_same_class <- all(sapply(formatted_data, is.factor))

  if (!all_data_same_class) {
    rlang::abort(
      message = "Input variables are expected to have the same class, but have different classes instead.",
      class = c("emp_test_error", "input_argument_error")
    )
  }

  # If input is integer, input variables should have the same length, and the
  # minimum values should be 0. Integer values represent the number of counts
  # for each class.
  if (is.integer(formatted_data[[1L]])) {
    if (!all(lengths(formatted_data) == length(formatted_data[[1L]]))) {
      rlang::abort(
        message = paste0(
          "Input variables with integer values are expected to represent the count for each category. ",
          "Thus all input variables should have the same length. However, lengths were found to differ: ",
          paste_s(lengths(formatted_data)), " for ", paste_s(formatted_data_names), " respectively."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    for (ii in seq_along(formatted_data)) {
      if (any(formatted_data[[ii]] < 0L)) {
        rlang::abort(
          message = paste0(
            "Input variables with integer values are expected to represent the count for each category, and cannot be negative. ",
            formatted_data_names[ii], " contains one or more negative values."
          ),
          class = c("emp_test_error", "input_argument_error")
        )
      }
    }

    has_names <- sapply(formatted_data, function(x) (!is.null(names(x))))
    if (!(all(has_names)) || !(all(!has_names))) {
      rlang::abort(
        message = paste0(
          "Input variables with integer values should either all be named, or all be unnamed. ",
          "Found: mix of variables with names and without names."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    if (all(has_names)) {
      if (!all(sapply(formatted_data, function(x, ref) (setequal(names(x), ref)), ref = names(formatted_data[[1L]])))) {
        rlang::abort(
          message = paste0(
            "Input variables with integer values are expected to have the same categories (names). ",
            "Found:  differ between variables."
          ),
          class = c("emp_test_error", "input_argument_error")
        )
      }

      if (!all(sapply(formatted_data, function(x, ref) (identical(names(x), ref)), ref = names(formatted_data[[1L]])))) {
        rlang::abort(
          message = paste0(
            "Input variables with integer values are expected to have the same categories (names). ",
            "Found: categories were ordered differently between variables."
          ),
          class = c("emp_test_error", "input_argument_error")
        )
      }
    }

    # Set categories
    if (all(has_names)) categories <- names(formatted_data[[1L]])
    else categories <- paste0("c", seq_along(formatted_data[[1L]]))

    # Apply categories as names.
    if (all(!has_names)) {
      for (ii in seq_along(formatted_data)) {
        names(formatted_data[[ii]]) <- categories
      }
    }
  }

  # If input is factor, ensure that the same levels appear in each variable,
  # and that their ordering is consistent.
  if (is.factor(formatted_data[[1L]])) {
    if (!all(sapply(formatted_data, function(x, ref) (setequal(levels(x), ref)), ref = levels(formatted_data[[1L]])))) {
      rlang::abort(
        message = paste0(
          "Categorical input variables are expected to have the same categories (levels). ",
          "Found: categories differ between variables."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    if (!all(sapply(formatted_data, function(x, ref) (identical(levels(x), ref)), ref = levels(formatted_data[[1L]])))) {
      rlang::abort(
        message = paste0(
          "Categorical input variables are expected to have the same categories (levels). ",
          "Found: categories were ordered differently between variables."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    # Set categories
    categories <- levels(formatted_data[[1L]])
  }

  # Logical input data.
  if (is.logical(formatted_data[[1L]])) {
    categories <- c(FALSE, TRUE)
  }

  # Character type input data.
  if (is.character(formatted_data[[1L]])) {
    categories <- unique(unlist(lapply(formatted_data, unique)))
  }

  # Convert input variables to counts of each class.
  if (!is.integer(formatted_data[[1L]])) {
    for (ii in seq_along(formatted_data)) {
      counts <- integer(length(categories))
      for (jj in seq_along(categories)) {
        counts[jj] <- sum(formatted_data[[ii]] == categories[jj])
      }
      names(counts) <- categories
      formatted_data[[ii]] <- counts
    }
  }

  # Set names.
  names(formatted_data) <- formatted_data_names

  return(formatted_data)
}



.check_input_data <- function(
  data,
  condition
) {

  # Check if condition.
  allow_more <- allow_fewer <- FALSE
  n_required_sets <- as.integer(condition[2L])
  if (condition[1L] == "exactly") {
    # pass

  } else if (condition[1L] == "at_most") {
    allow_fewer <- TRUE

  } else if (condition[1L] == "at_least") {
    allow_more <- TRUE

  } else {
    ..error_reached_unreachable_code(paste0("condition for format_data was malformed: ", condition[1L]))
  }

  if (!rlang::is_bare_list(data)) {
    ..error_reached_unreachable_code(paste0("data is not a list ", condition[1L]))
  }

  n_available_sets <- length(data)

  # Check that any data is present.
  if (n_available_sets == 0L) {
    rlang::abort(
      message = "No numeric data was found.",
      class = c("emp_test_error", "input_argument_error")
    )
  }

  # Check that none of the data has zero length.
  if (any(lengths(data) == 0L)) {
    variables_empty <- names(data)[lengths(data) == 0L]
    rlang::abort(
      message = paste0(
        "One or more empty variables were detected: ", paste_s(variables_empty)
      ),
      class = c("emp_test_error", "input_argument_error")
    )
  }

  # Check that the number of variables making up the input data is correct.
  if (!allow_more && !allow_fewer) {
    if (n_available_sets != n_required_sets) {
      rlang::abort(
        message = paste0(
          "Exactly ", n_required_sets, " numeric variables were expected. ",
          "Found: ", n_available_sets, " variables."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

  } else if (allow_more) {
    if (n_available_sets < n_required_sets) {
      rlang::abort(
        message = paste0(
          "At least ", n_required_sets, " numeric variables were expected. ",
          "Found: ", n_available_sets, " variables."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

  } else if (allow_fewer) {
    if (n_available_sets > n_required_sets) {
      rlang::abort(
        message = paste0(
          "At most ", n_required_sets, " numeric variables were expected. ",
          "Found: ", n_available_sets, " variables."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }
  }

  return(invisible(TRUE))
}



.check_paired_input_data <- function(
    data
) {
  variable_lengths <- lengths(data)
  if (!all(variable_lengths == variable_lengths[1L])) {
    rlang::abort(
      message = paste0(
        "Paired analysis expect variables with the same number of values. ",
        "Found: variables with ", paste_s(variable_lengths), " instances."
      ),
      class = c("emp_test_error", "input_argument_error")
    )
  }

  return(invisible(TRUE))
}
