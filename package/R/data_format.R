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
          formatted_data[[data_index]] <- data_set[[column]]
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
