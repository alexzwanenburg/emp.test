#' @include generic_test.R
#' @include generic_goodness_of_fit_test.R
NULL

#' Bootstrap categorical goodness-of-fit test
#'
#' Performs a goodness-of-fit test on categorical variables.
#'
#' @param ... One or more categorical variables that are to be tested. Different
#'   types are allowed. Vectors consisting of `logical`, `character` and
#'   `factor` type data are allowed. Vectors of `integer` type are also allowed
#'   if these represent counts of (named) categories.
#'
#'   If more than one categorical variable is provided, tests are conducted
#'   separately for each variable. Note however, that what is being tested
#'   depends on `expected`.
#' @param expected The expected distribution. In addition to the `logical`,
#'   `character`, `factor`, and `integer` (representing counts) types also
#'   allowed as input variable, `expected` can also be one of the following:
#'
#'   * A `numeric` vector, representing the fraction of each category.
#'
#'   * A random function that returns vectors of `logical`, `character`,
#'   `factor` and `integer` (indicating a category, not counts). This function
#'   is then sampled to identify the expected distribution. The **first**
#'   argument is expected to be the number of samples drawn.
#'
#'   * `NULL` (default): in this case, the expected distribution is considered
#'   to be the joined distribution of all input variables. This results in an
#'   *independence* test.
#'
#' @param expected_args A named list of additional arguments supplied to
#'   `expected`, if `expected` is a custom function.
#' @param test_statistic Test statistic computed from observed and expected
#'   data. `test_statistic` can be one of the following:
#'
#'   * `"chi_squared"`: computes Pearson's chi-squared test statistic.
#'
#'   * `"g"`: computes the g-test's test statistic.
#'
#'   * `"cressie_read"`: computes Cressie and Read's test statistic, of which
#'   both the chi-squared and g-test test statistics are special forms. This
#'   test statistic has a special parameter `lambda`. If not specificied using
#'   `test_statistic_args`, `lambda = 2/3` is used, after recommendations by
#'   Cressie and Read.
#'
#'   * A custom function that returns a single positive numeric value. The
#'   initial **two arguments** are expected to be counts of observed and
#'   expected categories, respectively.
#'
#'   * `NULL`: computes a distribution under the null-hypothesis and compares
#'   each category of the observations in a point-wise manner.
#' @param test_statistic_args A named list of arguments that is provided to
#'   `test_statistic`. For `"cressie_read"`, a `lambda` parameter can be
#'   provided.
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
  if (!is.null(test_statistic)) {
    # Test using test statistic.
    object <- methods::new(
      "empiricalCategoricalGOFTest",
      data = list(...),
      expected = expected,
      expected_args = expected_args,
      significance_level = significance_level,
      test_statistic = test_statistic,
      test_statistic_args = test_statistic_args
    )

  } else {
    # Point test.
    object <- methods::new(
      "empiricalCategoricalGOFPointTest",
      data = list(...),
      expected = expected,
      expected_args = expected_args,
      significance_level = significance_level,
      test_statistic = test_statistic
    )
  }

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

      # Determine which test statistic should be used.
      if (tolower(test_statistic) %in% c("chi", "chi_squared", "chi-squared", "chi.squared")) {
        test_statistic_fun <- test_statistic_chi_squared

      } else if (tolower(test_statistic) %in% c("g", "g_test", "g-test")) {
        test_statistic_fun <- test_statistic_g

      } else if (tolower(test_statistic) %in% c("cressie_read", "cressie-read", "cr")) {
        test_statistic_fun <- test_statistic_cressie_read

      } else {
        rlang::abort(
          message = paste0(
            "The provided test_statistic is expected to be one of 'chi_squared', 'g', 'cressie_read'. ",
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
          "test_statistic should be NULL, a function, or one of 'chi-squared', 'g', and 'cressie_read'.",
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
      .Object@name <- "bootstrap point-wise categorical goodness-of-fit test"

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

    # Add "all" test, which relies on the test-statistic computed against the
    # concatenated distribution.
    if (length(data) > 1L) {
      sampled_data[["all"]] <- list(
        "observed" = c(lapply(sampled_data, function(x) (x$observed)), recursive = TRUE),
        "expected" = c(lapply(sampled_data, function(x) (x$expected)), recursive = TRUE)
      )
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
    values <- lapply(
      samples,
      function(x, object) {
        .compute_test_statistic(
          object = object,
          observed = x$observed,
          expected = x$expected
        )
      },
      object = object
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
    if (!is.function(object@summary_function)) {
      ..error_reached_unreachable_code("test_statistic should be a function")
    }

    return(do.call(
      object@summary_function,
      args = c(
        list(observed, expected),
        object@summary_function_args
      )
    ))
  }
)



## ..bootstrap_test ------------------------------------------------------------
setMethod(
  "..bootstrap_test",
  signature(object = "empiricalCategoricalGOFTest"),
  function(
    object,
    random_values,
    ...
  ) {
    data <- list()
    index_start <- 1L

    # Iterate over variables.
    for (group in names(object@input_data)) {
      input_data <- object@input_data[[group]]
      n_samples <- sum(input_data)
      index_end <- index_start + n_samples  - 1L

      # The observed attribute contains a numeric vector with counts, sampled
      # from the original data.
      observed <- integer(n_samples) + 1L
      probability <- cumsum(input_data / n_samples)
      probability <- probability[1L:(length(probability) - 1L)]
      for (p in probability) {
        observed <- observed + as.integer(random_values[index_start:index_end] > p)
      }

      # Convert to counts
      observed <- .convert_vector_to_counts(
        observed,
        categories = names(input_data)
      )

      # Set data.
      data[[group]] <- list(
        "observed" = observed,
        "expected" = object@expected * n_samples,
        "null" = object@summary_data[[group]]
      )
    }

    # Add all
    if (length(object@input_data) > 1L) {
      data[["all"]] <- list(
        "observed" = c(lapply(data, function(x) (x$observed)), recursive = TRUE),
        "expected" = c(lapply(data, function(x) (x$expected)), recursive = TRUE),
        "null" = object@summary_data$all
      )
    }

    return(.test(
      object = object,
      data = data,
      hypothesis_test_only = TRUE
    ))
  }
)



## .test -----------------------------------------------------------------------
setMethod(
  ".test",
  signature(object = "empiricalCategoricalGOFTest"),
  function(
    object,
    data = NULL,
    hypothesis_test_only = FALSE,
    ...
  ) {
    test_output <- list()

    if (is.null(data)) {
      # Collect data for point-estimates.
      data <- list()
      for (group in names(object@input_data)) {
        data[[group]] <- list(
          "observed" = object@input_data[[group]],
          "expected" = object@expected * sum(object@input_data[[group]]),
          "null" = object@summary_data[[group]]
        )
      }

      # Add all
      if (length(object@input_data) > 1L) {
        data[["all"]] <- list(
          "observed" = c(lapply(data, function(x) (x$observed)), recursive = TRUE),
          "expected" = c(lapply(data, function(x) (x$expected)), recursive = TRUE),
          "null" = object@summary_data$all
        )
      }
    }

    # Iterate over individual of data.
    for (group in names(data)) {
      # Compute test statistic for the input (or bootstrapped input) data.
      obs_statistic <- .compute_test_statistic(
        object = object,
        observed = data[[group]]$observed,
        expected = data[[group]]$expected
      )

      results <- ..test(
        object = object,
        x_0 = obs_statistic,
        x_1 = data[[group]]$null,
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

    # Case when a test-statistic is computed. The p-value is roughly the
    # number of times the bootstrapped test statistic under the NULL
    # hypothesis exceeds the observed test statistic: x_1 >= x_0.
    x <- sort(x_0 - x_1)

    # Compute confidence interval.
    ci <- stats::quantile(x, probs = 1.0 - object@significance_level, type = 8L, names = FALSE)

    # Reject the null hypothesis if ci >= 0
    reject_null <- ci[1L] >= 0.0

    if (hypothesis_test_only) {
      return(list("reject_null" = reject_null))
    }

    # Compute percentiles on x. Note that this corresponds to type = 8.
    p <- (seq_along(x_1) - 1.0 / 3.0) / (length(x_1) + 1.0 /3.0)
    p_value <- suppressWarnings(
      stats::approx(x = x, y = p, xout = 0.0, rule = 2L)$y
    )

    return(list(
      "expected" = object@expected,
      "test_statistic_observed" = x_0,
      "test_statistic_critical" = stats::quantile(x_1, object@significance_level, names = FALSE),
      "p_value" = p_value,
      "reject_null" = reject_null
    ))
  }
)



# empiricalCategoricalGOFPointTest object --------------------------------------
setClass(
  "empiricalCategoricalGOFPointTest",
  contains = "empiricalCategoricalGOFTest"
)



## .sample_data ----------------------------------------------------------------
setMethod(
  ".sample_data",
  signature(object = "empiricalCategoricalGOFPointTest"),
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
      sampled_data[[variable]] <- observed_count

      # Increment random value index counter.
      index_start <- index_end + 1L
    }

    return(sampled_data)
  }
)


## ..compute_summary_statistic -------------------------------------------------
setMethod(
  "..compute_summary_statistic",
  signature(object = "empiricalCategoricalGOFPointTest"),
  function(
    object,
    ...
  ) {
    # Produce samples under the null-hypothesis.
    samples <- .sample_data(object, ...)

    return(samples)
  }
)
