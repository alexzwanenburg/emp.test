# These functions were directly ported from the familiar R package.

.check_is_numeric <- function(
    x,
    var_name,
    check_finite = TRUE,
    call = rlang::caller_env()
) {
  if (!is.numeric(x)) {
    ..error_type_not_valid(
      x = x,
      var_name = var_name,
      valid_type = "numeric",
      call = call
    )
  }

  if (check_finite) {
    if (any(!is.finite(x))) {
      rlang::abort(
        message = paste0(var_name, " is expected to contain only finite numeric values."),
        class = c("emp_test_error", "input_argument_error"),
        call = call
      )
    }
  }

  return(invisible(TRUE))
}



.check_is_integer <- function(
    x,
    var_name,
    call = rlang::caller_env()
) {
  if (!is.integer(x)) {
    ..error_type_not_valid(
      x = x,
      var_name = var_name,
      valid_type = "integer",
      call = call
    )
  }

  return(invisible(TRUE))
}



.check_number_in_valid_range <- function(
    x,
    var_name,
    range,
    closed = c(TRUE, TRUE),
    call = rlang::caller_env()
) {

  # Interpret single input range value as range containing only one value.
  if (length(range) == 1L) range <- c(range, range)

  # Set lower limit to -Inf
  if (is.na(range[1L])) range[1L] <- -Inf

  # Set upper limit to Inf
  if (is.na(range[2L])) range[2L] <- Inf

  # Some internal checks that should never be triggered.
  if (length(x) != 1L) {
    ..error_reached_unreachable_code(paste0(
      ".check_number_in_valid_range: x does not have length 1."
    ))
  }

  # Another internal checks that should never be triggered.
  if (range[2L] - range[1L] < 0.0) {
    ..error_reached_unreachable_code(paste0(
      ".check_number_in_valid_range: the range is inverted"
    ))
  }

  # Check that x is numeric or NA.
  if (!is.numeric(x) && !is.na(x)) {
    ..error_type_not_valid(
      x = x,
      var_name = var_name,
      valid_type = "numeric",
      call = call
    )
  }

  if (!is.na(x)) {
    is_outside_range <- ifelse(
      closed[1L],
      x < range[1L],
      x <= range[1L]
    ) || ifelse(
      closed[2L],
      x > range[2L],
      x >= range[2L]
    )

  } else {
    # NA-values are outside the valid range.
    is_outside_range <- TRUE
  }

  if (is_outside_range) {
    ..error_value_outside_allowed_range(
      x = x,
      var_name = var_name,
      range = range,
      call = call
    )
  }

  return(invisible(TRUE))
}



.check_argument_length <- function(
    x,
    var_name,
    min = 0L,
    max = Inf,
    call = rlang::caller_env()
) {
  if (length(x) < min) {
    ..error_variable_has_too_few_values(
      x = x,
      var_name = var_name,
      req_length = c(min, max),
      call = call
    )

  } else if (length(x) > max) {
    ..error_variable_has_too_many_values(
      x = x,
      var_name = var_name,
      req_length = c(min, max),
      call = call
    )
  }

  return(invisible(TRUE))
}



.check_parameter_value_is_valid <- function(
    x,
    var_name,
    values,
    call = rlang::caller_env()
) {

  # Check if NULL is an allowed value
  null_allowed <- any(is.null(values))

  if (length(x) == 1L) {

    # Check if x is NULL
    if (is.null(x) && null_allowed) {
      # If x is NULL and this is allowed, return to parent function
      return(invisible(TRUE))
    }
  }

  if (any(is.null(x)) && !null_allowed) {
    # If any of x is NULL and this is not allowed, raise an error.
    ..error_value_not_allowed(
      x = x,
      var_name = var_name,
      values = values,
      call = call
    )
  }

  if (!all(x %in% values)) {
    # Throw an error if any x is not among the valid values
    ..error_value_not_allowed(
      x = x,
      var_name = var_name,
      values = values,
      call = call
    )
  }

  return(invisible(TRUE))
}



# The functions below are new in emp.test.
.check_is_function <- function(
    x,
    var_name,
    call = rlang::caller_env()
) {
  if (!is.function(x)) {
    ..error_type_not_valid(
      x = x,
      var_name = var_name,
      valid_type = "function",
      call = call
    )
  }

  return(invisible(TRUE))
}
