#' @include generic_test.R
#' @include generic_goodness_of_fit_test.R
NULL



# empiricalCategoricalGOFTest object -------------------------------------------
setClass(
  "empiricalCategoricalGOFTest",
  contains = "empiricalGenericGOFTest",
  prototype = list(
    percentiles = NA_real_,
    expected = NA_real_
  )
)



## initialize ------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalCategoricalGOFTest"),
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
      .Object@summary_function <- test_statistic
      .Object@summary_function_args <- test_statistic_args

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
    } else if (is.character(test_statistic)) {

      # Determine which
      if (test_statistic == "chi_square") {
        test_statistic_fun <- test_statistic_chi_square
      } else if (test_statistic == "g") {
        test_statistic_fun <- test_statistic_g
      } else if (test_statistic == "cressie_read") {
        test_statistic_fun <- test_statistic_cressie_read
      } else {
        rlang::abort(
          message = paste0(
            "The provided test_statistic is expected to be one of 'chi_square', 'g', 'cressie_read'. ",
            "Found: ", test_statistic, "."
          )
        )
      }

      # Assign as function.
      .Object@summary_function <- test_statistic_fun
      .Object@summary_function_args <- test_statistic_args

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


## get_n_draw_input_samples ----------------------------------------------------
setMethod(
  "get_n_draw_input_samples",
  signature(
    object = "empiricalCategoricalGOFTest"
  ),
  function(object) {
    if (is.null(object@input_data)) return(0L)

    # The default is that expected is generated for all input data..
    return(sum(sapply(object@input_data, sum)))
  }
)


## get_n_draw_summary_samples --------------------------------------------------
setMethod(
  "get_n_draw_summary_samples",
  signature(object = "empiricalCategoricalGOFTest"),
  function(object) {
    if (is.null(object@summary_data)) return(0L)

    # The default is for all summary data to be resampled.
    # TODO: determine how to do this effectively. Do we want to compute post-hoc
    # power for a goodness-of-fit test?
    browser()
    return(sum(lengths(object@summary_data)))
  }
)


## .sample_data ----------------------------------------------------------------
setMethod(
  ".sample_data",
  signature(object = "empiricalPairedDiffTest"),
  function(object, random_values, ...) {
    data <- object@summary_data
    if (is.null(data)) data <- object@input_data
    if (is.null(random_values)) return(data)

    # Special case for point estimates.
    if (is.null(random_values)) return(object@input_data)

    # For paired tests the same samples should be randomly selected.
    n_samples <- length(data[[1L]])
    sample_indices <- as.integer(ceiling(random_values[1L:n_samples] * n_samples))

    # These edge cases are highlu unlikely but not impossible.
    sample_indices[sample_indices < 1L] <- 1L
    sample_indices[sample_indices > n_samples] <- n_samples

    # Select samples.
    sampled_data <- list()
    for (variable in names(data)) {
      sampled_data[[variable]] <- data[[variable]][sample_indices]
    }

    return(sampled_data)
  }
)


## ..compute_summary_statistic -------------------------------------------------
setMethod(
  "..compute_summary_statistic",
  signature(object = "empiricalCategoricalGOFTest"),
  function(
    object,
    ...
  ) {
    # Produce samples. For categorical goodness-of-fit tests, this
    # is a vector of input observed data with a matched (random) sample of
    # expected.
    samples <- .sample_data(object, ...)

    # Compute test statistics
    values <- mapply(
      FUN = .compute_test_statistic,
      observed = samples$observed,
      expected = samples$expected,
      MoreArgs = list("object" = object),
      SIMPLIFY = FALSE,
      USE.NAMES = TRUE
    )

    return(values)
  }
)



## .compute_test_statistic -----------------------------------------------------
setMethod(
  ".compute_test_statistic",
  signature(object = "empiricalCategoricalGOFTest"),
  function(
    object,
    observed,
    expected
  ) {
    if (!is.function(object@test_statistic)) {
      ..error_reached_unreachable_code("test_statistic should be a function")
    }

    return(do.call(
      object@summary_function,
      args = C(
        list(observed, expected),
        object@summary_function_args
      )
    ))
  }
)
