.get_tolerance_bounds <- function(
  object = object,
  x_1,
  x_2 = NULL
) {
  if (!class(object) %in% c("empiricalEquivalenceTest", "empiricalEquivalenceLocTest")) {
    ..error_reached_unreachable_code("object should be empiricalEquivalenceTest or empiricalEquivalenceLocTest. Found: ", class(object))
  }

  # Initial values
  tolerance_lower <- tolerance_upper <- NULL

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

    if (is.null(x_2)) {
      # used in one-sample tests
      tolerance <- mapply(
        FUN = object@tolerance,
        x_1,
        MoreArgs = object@tolerance_function_args,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )

    } else {
      # used in two-sample tests
      tolerance <- mapply(
        FUN = object@tolerance,
        x_1,
        x_2,
        MoreArgs = object@tolerance_function_args,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
    }

    if (object@alternative == "equivalent") {
      if (length(tolerance[[1L]]) > 1L) {
        tolerance_lower <- sapply(tolerance, function(x) (x[1L]))
        tolerance_upper <- sapply(tolerance, function(x) (x[2L]))
      } else {
        tolerance_upper <- sapply(tolerance, function(x) (x[1L]))
        tolerance_lower <- -tolerance_upper
      }
    } else if (object@alternative == "not_inferior") {
      tolerance_lower <- sapply(tolerance, function(x) (x[1L]))

    } else if (object@alternative == "not_superior") {
      if (length(tolerance[[1L]]) > 1L) {
        tolerance_upper <- sapply(tolerance, function(x) (x[2L]))
      } else {
        tolerance_upper <- sapply(tolerance, function(x) (x[1L]))
      }

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }
  }

  return(list(
    "tolerance_lower" = tolerance_lower,
    "tolerance_upper" = tolerance_upper
  ))
}
