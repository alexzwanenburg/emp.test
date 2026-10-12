#' @include generic_test.R
#' @include generic_goodness_of_fit_test.R
NULL

# empiricalMultinomialGOFTest object -----------------------------------------------
setClass(
  "empiricalMultinomialGOFTest",
  contains = "empiricalGenericGOFTest",
  prototype = list(
    percentiles = NA_real_,
    expected = NA_real_,
    test_statistic = NULL,
    test_statistic_args = NULL
  )
)


## initialize ------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalMultinomialGOFTest"),
  function(
    .Object,
    data,
    expected,
    expected_args = NULL,
    test_statistic,
    test_statistic_args = NULL,
    ...
  ) {
    # Pass to empiricalTest for global attributes.
    .Object <- callNextMethod()

    if (rlang::is_missing(data)) ..error_missing_variable("data")
    if (rlang::is_missing(expected)) ..error_missing_variable("expected")
    if (rlang::is_missing(expected_args)) ..error_missing_variable("expected_args")
    if (rlang::is_missing(test_statistic)) ..error_missing_variable("test_statistic")
    if (rlang::is_missing(test_statistic_args)) ..error_missing_variable("test_statistic_args")

    # Format data.
    .Object@input_data <- format_data_categorical(data = data)

    # Check expected.
    .Object@expected <- .set_expected_categorical(
      expected = expected,
      data = .Object@input_data,
      expected_args = expected_args
    )

    # Check test_statistic
    if (is.function(test_statistic)) {
      .Object@test_statistic <- test_statistic
      .Object@test_statistic_args <- test_statistic_args

      statistic <- tryCatch(
        .compute_test_statistic(
          object = .Object,
          observed = .Object@input_data[[1L]],
          expected = .Object@input_data[[1L]] + 1L  # This is dummy data for testing only.
        ),
        error = identity
      )

      if (inherits(statistic, "error")) {
        rlang::abort(
          message = paste0(
            "The provided test_statistic function produced an error. Found: ",
            as.character(statistic)
          ),
          class = c("emp_test_error", "input_argument_error")
        )
      }

      if (!is.numeric(statistic)) {
        rlang::abort(
          message = paste0(
            "The provided test_statistic is expected to yield a single numeric value. ",
            "Found: output of class ", paste_s(class(statistic)), "."
          )
        )
      }

      if (length(statistic) > 1L) {
        rlang::abort(
          message = paste0(
            "The provided test_statistic is expected to yield a single numeric value. ",
            "Found: ", length(statistic), " values."
          )
        )
      }

    } else if (is.null(test_statistic)) {
      # pass --> Use bootstrap tests directly.
    } else if (test_statistic %in% c("chi_square", "g", "cressie_read")) {
      browser()

      if (test_statistic == "chi_square") test_statistic_fun <- test_statistic_chi_square
      else if (test_statistic == "g") test_statistic_fun

      # TODO: assign functions instead of names.
      .Object@test_statistic <- test_statistic_fun
      .Object@test_statistic_args <- test_statistic_args

    } else {
      rlang::abort(
        message = paste0(
          "test_statistic should be NULL, a function, or one of 'chi-square', 'g', and 'cressie_read'.",
          "Found: an invalid value for test_statistic."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    return(.Object)
  }
)



## .compute_test_statistic -----------------------------------------------------
setMethod(
  ".compute_test_statistic",
  signature(.Object = "empiricalGenericGOFTest"),
  function(
    object,
    observed,
    expected
  ) {
    if (!is.function(object@test_statistic)) {
      ..error_reached_unreachable_code("test_statistic should be a function")
    }

    return(do.call(
      object@test_statistic,
      args = C(
        list(observed, expected),
        object@test_statistic_args
      )
    ))

  }
)
