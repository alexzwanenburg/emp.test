# These functions were directly ported from the familiar R package.

..error_variable_has_too_few_values <- function(
    x,
    var_name,
    req_length = 1L,
    allow_more = FALSE,
    call = rlang::caller_env()
) {

  # Handle req_length being a vector with two values.
  if (length(req_length) == 2L) {
    if (!is.finite(req_length[2L])) {
      req_length <- req_length[1L]
      allow_more <- TRUE

    } else if (diff(req_length) == 0L) {
      req_length <- req_length[1L]
      allow_more <- FALSE
    }
  }

  if (length(req_length) == 1L) {
    # The req_length argument specifies the exact number or the minimum number
    # of values.
    message_string <- paste0(
      "The ", var_name, " argument requires ",
      ifelse(allow_more, "at least ", "exactly "), req_length,
      ifelse(req_length == 1L, " value", " values"), ". ",
      length(x), ifelse(length(x) == 1L, " value was", " values were"), " found."
    )

  } else {
    # The req_length argument specifies a range for the number of values.
    message_string <- paste0(
      "The ", var_name, " argument requires between ",
      req_length[1L], " and ", req_length[2L], " values. ",
      ifelse(length(x) == 1L, " value was", " values were"), " found."
    )
  }

  rlang::abort(
    message = message_string,
    class = c("emp_test_error", "input_argument_error"),
    call = call
  )
}



..error_variable_has_too_many_values <- function(
    x,
    var_name,
    req_length = 1L,
    allow_fewer = FALSE,
    call = rlang::caller_env()
) {

  # Handle req_length being a vector with two values.
  if (length(req_length) == 2L) {
    if (!is.finite(req_length[1L])) {
      req_length <- req_length[2L]
      allow_fewer <- TRUE

    } else if (diff(req_length) == 0L) {
      req_length <- req_length[2L]
      allow_fewer <- FALSE
    }
  }

  if (length(req_length) == 1L) {
    # The req_length argument specifies the exact number or the maximum number
    # of values.
    message_string <- paste0(
      "The ", var_name, " argument requires ",
      ifelse(allow_fewer, "at most ", "exactly "), req_length,
      ifelse(req_length == 1L, " value", " values"), ". ",
      length(x), ifelse(length(x) == 1L, " value was", " values were"), " found."
    )

  } else {
    message_string <- paste0(
      "The ", var_name, " argument requires between ",
      req_length[1L], " and ", req_length[2L], " values. ",
      ifelse(length(x) == 1L, " value was", " values were"), " found."
    )
  }

  rlang::abort(
    message = message_string,
    class = c("emp_test_error", "input_argument_error"),
    call = call
  )
}



..error_value_outside_allowed_range <- function(
    x,
    var_name,
    range,
    call = rlang::caller_env()
) {

  # Determine the string for the allowed value.
  if (is.infinite(range[1L]) && is.infinite(range[2L])) {
    allowed_value_text <- "be any real number"

  } else if (is.infinite(range[1L])) {
    allowed_value_text <- paste("have a value of", range[2L], "or less")

  } else if (is.infinite(range[2L])) {
    allowed_value_text <- paste("have a value of", range[1L], "or more")

  } else if (range[1L] - range[2L] == 0.0) {
    allowed_value_text <- paste("be exactly", range[1L])

  } else {
    allowed_value_text <- paste("have a value between", range[1L], "and", range[2L])
  }

  message_string <- paste0(
    var_name, " is expected to ", allowed_value_text,
    ". Found: ", paste_s(x)
  )

  rlang::abort(
    message = message_string,
    class = c("emp_test_error", "input_argument_error"),
    call = call
  )
}



..error_value_not_allowed <- function(
    x,
    var_name,
    values,
    call = rlang::caller_env()
) {

  # Replace NULL in x by "NULL"
  x[is.null(x)] <- "NULL"

  # Replace NULL in values by "NULL"
  values[is.null(values)] <- "NULL"

  # Identify those values that are not allowed
  forbidden_values <- setdiff(x, values)

  message_string <- paste0(
    var_name, " expects one of ", paste_s(values), " as input. ",
    "Found the following unknown values: ", paste_s(forbidden_values)
  )

  rlang::abort(
    message = message_string,
    class = c("emp_test_error", "input_argument_error"),
    call = call
  )
}



..error_type_not_valid <- function(
    x,
    var_name,
    valid_type,
    call = rlang::caller_env()
) {

  message_string <- paste0(
    var_name, " has type ", paste_s(class(x)),
    " whereas ", paste_s(valid_type), " was expected."
  )

  rlang::abort(
    message = message_string,
    class = c("emp_test_error", "input_argument_error"),
    call = call
  )
}


..error_reached_unreachable_code <- function(..., call = rlang::caller_env()) {
  message_string <- paste0(
    "This error should not occur, as this code should not be reachable by design. ",
    "Contact the package maintainer if you see this error.\n\t",
    paste0(list(...), collapse = "")
  )

  rlang::abort(
    message = message_string,
    class = c("emp_test_error", "developer_error"),
    call = call
  )
}
