#' @include generic_test.R
#' @include generic_goodness_of_fit_test.R
NULL

#' Bootstrap categorical goodness-of-fit test
#'
#' Performs a goodness-of-fit test on categorical variables.
#'
#' @param ... One or more counts of categorical variables that are to be tested.
#'   If more than one categorical variable is provided, tests are conducted
#'   separately for each variable. Note however, that what is being tested
#'   depends on `expected`.
#' @param expected descriptions
#' @param expected_args A named list of additional arguments supplied to
#'   `expected`, if `expected` is a custom function. The **first** argument of
#'   `expected` is expected to indicate the number of samples to be drawn.
#' @param test_statistic description
#' @param test_statistic_args A named list of arguments that is provided to
#'   `test_statistic`.
#' @inheritParams difference_test
#'
#' @details A goodness-of-fit test assesses how closely observed data matches
#'   expected data or distributions.
#'
#'   The bootstrap categorical goodness-of-fit tests is the empirical equivalent
#'   of statistical tests such as the chi-square or g-test.
#'
#' @returns htest object
#' @export
categorical_goodness_of_fit_test <- function(
    ...,
    expected = NULL,
    expected_args = NULL,
    significance_level = 0.95,
    test_statistic = "cressie_read",
    test_statistic_args = NULL
) {
  # Create test object.
  object <- methods::new(
    "empiricalCategoricalGOFTest",
    data = list(...),
    expected = expected,
    expected_args = expected_args,
    significance_level = significance_level,
    test_statistic = test_statistic,
    test_statistic_args = test_statistic_args
  )

  # Set the number of bootstraps.
  object@n_bootstraps <- 1000L

  # Perform the test.
  results <- run_test(object)

  # Return test information.
  return(results@htest)
}


# empiricalCategoricalGOFTest object -------------------------------------------
setClass(
  "empiricalCategoricalGOFTest",
  contains = "empiricalGenericGOFTest"
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

    # Determine the number of samples that need to be drawn.
    .Object@n_draw_input_samples <- get_n_draw_input_samples(.Object)

    # Check and set expected.
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

    # Set name
    if (is.character(test_statistic)) {
      test_statistic <- gsub("_", "-", test_statistic)
      .Object@name <- paste0(
        "bootstrap categorical goodness-of-fit hypothesis (", test_statistic, ") test"
      )
    } else if (is.null(test_statistic)) {
      .Object@name <- "bootstrap categorical goodness-of-fit test"

    } else {
      .Object@name <- "bootstrap categorical goodness-of-fit hypothesis test"
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

    # The default is that observed and expected is generated for all input data.
    return(sum(sapply(object@input_data, sum)))
  }
)


## get_n_draw_summary_samples --------------------------------------------------
setMethod(
  "get_n_draw_summary_samples",
  signature(object = "empiricalCategoricalGOFTest"),
  function(object) {
    if (is.null(object@input_data)) return(0L)

    # Unlike estimation, categorical goodness-of-fit tests resample observed
    # data instead of the summary data. Summary data (containing the data under
    # the Null hypothesis) remains fixed.
    return(sum(sapply(object@input_data, sum)))
  }
)


## .sample_data ----------------------------------------------------------------
setMethod(
  ".sample_data",
  signature(object = "empiricalCategoricalGOFTest"),
  function(object, random_values, ...) {
    # Produce samples. For categorical goodness-of-fit tests, this
    # is a vector of input observed data with a matched (random) sample of
    # expected.
    data <- object@input_data
    if (is.null(random_values)) {
      ..error_reached_unreachable_code("random_values cannot be NULL as expected is always randomised.")
    }

    # Create vectors of expected.
    n_samples <- sum(sapply(object@input_data, sum))
    if (!is.numeric(object@expected)) {
      ..error_reached_unreachable_code("The expected attribute should either be a function or a numeric vector.")
    }
    # The observed attribute contains a numeric vector with probabilities.
    observed <- integer(n_samples) + 1L

    # Find probability thresholds for each category. The last category is
    # dropped.
    probability <- cumsum(object@expected)
    probability <- probability[1L:(length(probability) - 1L)]
    for (p in probability) {
      observed <- observed + as.integer(random_values > p)
    }

    # Create observed and expected data based on samples.
    sampled_data <- list()
    index_start <- 1L
    for (variable in names(data)) {
      n_samples <- sum(data[[variable]])
      index_end <- index_start + n_samples  - 1L

      # Observed is based directly on random_values.
      observed_count <- .convert_vector_to_counts(
        observed[index_start:index_end],
        categories = names(data[[variable]])
      )

      # Expected is based on the expected attribute, which contains
      # probabilities.
      sampled_data[[variable]] <- list(
        "observed" = observed_count,
        "expected" = object@expected * n_samples
      )

      # Increment random value index counter.
      index_start <- index_end + 1L
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



## .bootstrap_test -------------------------------------------------------------
setMethod(
  ".bootstrap_test",
  signature(object = "empiricalCategoricalGOFTest"),
  function(
    object,
    bootstraps
  ) {
    # UPDATE
    browser()

    # Draw random values, and then split values for further dispatching.
    r <- .start_random_number_stream(seed = bootstraps[1L] + 1L)
    n_draw <- length(bootstraps) * object@n_draw_summary_samples
    random_values <- rstream::rstream.sample(r, n_draw)

    # Create chunks of random values. Note that this method is faster than
    # structure(split(..., cut(...))).
    random_value_chunks <- list()
    index_start <- 1L
    for (ii in seq_along(bootstraps)) {
      random_value_chunks[[ii]] <- random_values[index_start:(index_start + object@n_draw_summary_samples - 1L)]
      index_start <- index_start + object@n_draw_summary_samples
    }

    # Dispatch to per-bootstrap routines.
    results <- mapply(
      FUN = ..bootstrap_test,
      random_values = random_value_chunks,
      MoreArgs = list("object" = object),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )

    return(results)
  }
)


## ..bootstrap_test ------------------------------------------------------------
setMethod(
  "..bootstrap_test",
  signature(object = "empiricalCategoricalGOFTest"),
  function(
    object,
    ...
  ) {
    # UPDATE
    browser()
    # Produce samples.
    samples <- .sample_data(object, ...)

    return(.test(
      object = object,
      data = samples,
      hypothesis_test_only = FALSE
    ))
  }
)



## .test -----------------------------------------------------------------------
setMethod(
  ".test",
  signature(object = "empiricalCategoricalGOFTest"),
  function(
    object,
    data,
    hypothesis_test_only = FALSE,
    ...
  ) {
    browser()
    test_output <- list()

    # Iterate over individual of data.
    for (group in names(data)) {

      # Compute
      obs_statistic <- .compute_test_statistic(
        object = object,
        observed = object@input_data[[group]],
        expected = object@expected * sum(object@input_data[[group]])
      )

      results <- ..test(
        object = object,
        x_0 = obs_statistic,
        x_1 = data[[group]],
        hypothesis_test_only = hypothesis_test_only
      )
      test_output[[group]] <- results
    }

    return(test_output)
  }
)


## ..test ----------------------------------------------------------------------
setMethod(
  "..test",
  signature(object = "empiricalCategoricalGOFTest"),
  function(
    object,
    x_0,
    x_1,
    hypothesis_test_only = FALSE,
    ...
  ) {

    if (is.function(object@summary_function)) {
      # Case when a test-statistic is computed.

    } else {
      # Case when data are evaluated directly.

    }



    # Compute difference between groups. Pre-sort so that both quantile
    # and p-value computation can work with sorted data.
    x <- sort(x_1)

    return(..test_difference(
      object = object,
      x = x,
      hypothesis_test_only = hypothesis_test_only,
      location = object@location
    ))
  }
)
