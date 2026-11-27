#' @include generic_test.R
#' @include generic_goodness_of_fit_test.R
NULL

#' Bootstrap goodness-of-fit test
#'
#' Performs a goodness-of-fit test on continuous variables.
#'
#' @param ... One or more continuous variables that are to be tested. Different
#'   types are allowed. Vectors consisting of `numeric` type data are allowed.
#'
#'   If more than one numeric variable is provided, tests are conducted
#'   separately for each variable. Note however, that what is being tested
#'   depends on `expected`.
#' @param expected The expected distribution. In addition to `numeric` values
#'   (forming an empiric distribution), `expected` can also be one of
#'   the following:
#'
#'   * A quantile function that returns `numeric` vectors . This function
#'   is then sampled to identify the expected distribution. The
#'   argument is expected to be a vector of quantile probabilities named `p`,
#'   such as `stats::qnorm`.
#'
#'   * `NULL` (default): in this case, the expected distribution is considered
#'   to be the joined distribution of all input variables. This results in an
#'   *independence* test.
#'
#' @param expected_args A named list of additional arguments supplied to
#'   `expected`, if `expected` is a custom function.
#' @param alternative The alternative hypothesis. One of `"two_sided"`
#'   (default), `"greater"` and `"less"`. This parameter is only used if the
#'   test is performed point-wise (`test_statistic = NULL`).
#' @param test_statistic Test statistic computed from observed and expected
#'   data. `test_statistic` can be one of the following:
#'
#'   * `"cramer-von-mises"`: computes the Cramér-Von Mises test statistic.
#'
#'   * `"anderson-darling"`: computes the Anderson-Darling test statistic.
#'#'
#'   * `"kolmogorov-smirnov"` (default): Computes the Kolmogorov-Smirnov test statistic.
#'
#'   * `"kuiper"`: Computes Kuiper's test statistic.
#'
#'   * A custom function that returns a single positive numeric value. The
#'   initial **two arguments** are expected to be observed numeric values and
#'   expected numeric values, respectively.
#'
#'   * `NULL`: computes a distribution under the null-hypothesis and compares
#'   each category of the observations in a point-wise manner.
#' @param test_statistic_args A named list of arguments that is provided to
#'   `test_statistic`.
#' @inheritParams difference_test
#'
#' @details A goodness-of-fit test assesses how closely observed data matches
#'   expected data or distributions.
#'
#'   The bootstrap goodness-of-fit test is the empirical equivalent
#'   of statistical tests such as normality tests.
#'
#' @returns htest object
#' @export
numerical_goodness_of_fit_test <- function(
    ...,
    expected = NULL,
    expected_args = NULL,
    alternative = "two_sided",
    significance_level = 0.95,
    test_statistic = "kolmogorov-smirnov",
    test_statistic_args = NULL
) {
  # Summary data is used unchanged for determining confidence intervals, and
  # thus can be cached.
  on.exit(.clear_emp_test_global_env())

  # Create test object.
  if (!is.null(test_statistic)) {
    # Test using test statistic.
    object <- methods::new(
      "empiricalNumericalGOFTest",
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
      "empiricalNumericalGOFPointTest",
      data = list(...),
      expected = expected,
      expected_args = expected_args,
      alternative = alternative,
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



#' Point-wise numerical goodness-of-fit bootstrap test
#'
#' Performs a point-wise goodness-of-fit test on numeric variables.
#'
#' @param alternative The alternative hypothesis. One of `"two_sided"`
#'   (default), `"greater"` and `"less"`.
#' @inheritParams numerical_goodness_of_fit_test
#'
#' @details A goodness-of-fit test assesses how closely observed data matches
#'   expected data or distributions.
#'
#'   The bootstrap categorical goodness-of-fit tests is the empirical equivalent
#'   of statistical tests such as the chi-square or g-test.
#'
#' @returns htest object
#' @export
pointwise_numerical_goodness_of_fit_test <- function(
    ...,
    expected = NULL,
    expected_args = NULL,
    alternative = "two_sided",
    significance_level = 0.95
) {
  # Thin wrapper around numerical_goodness_of_fit_test
  return(
    numerical_goodness_of_fit_test(
      ...,
      expected = expected,
      expected_args = expected_args,
      alternative = alternative,
      significance_level = significance_level,
      test_statistic = NULL,
      test_statistic_args = NULL
    )
  )
}




# empiricalNumericalGOFTest object -------------------------------------------
setClass(
  "empiricalNumericalGOFTest",
  contains = "empiricalGenericGOFTest",
  slots = list(
    alternative = "character"
  ),
  prototype = list(
    alternative = "less"
  )
)



## initialize ------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalNumericalGOFTest"),
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
    .Object@input_data <- format_data(data = data)

    # Determine the number of samples that need to be drawn.
    .Object@n_draw_input_samples <- get_n_draw_input_samples(.Object)

    # Check and set expected.
    .Object@expected <- .set_expected_numerical(
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
          expected = .Object@input_data[[1L]] + 0.5  # This is dummy data for testing only.
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
      if (tolower(test_statistic) %in% c("cramer-von-mises", "cramer_von_mises", "cvm")) {
        test_statistic_fun <- test_statistic_cramer_von_mises

      } else if (tolower(test_statistic) %in% c("anderson-darling", "anderson_darling", "ad")) {
        test_statistic_fun <- test_statistic_anderson_darling

      } else if (tolower(test_statistic) %in% c("ks", "kolmogorov_smirnov", "kolmogorov-smirnov")) {
        test_statistic_fun <- test_statistic_ks

      } else if (tolower(test_statistic) %in% c("kuiper")) {
        test_statistic_fun <- test_statistic_kuiper

      } else if (tolower(test_statistic) %in% c("mean-residual-error", "mean_residual_error", "mre")) {
        test_statistic_fun <- test_statistic_mean_residual_error

      } else {
        rlang::abort(
          message = paste0(
            "test_statistic should be NULL, a function, or one of 'cramer-von-mises', ",
            "'anderson-darling', 'kolmogorov-smirnov', 'kuiper' and 'mean-residual-error'.",
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
          "test_statistic should be NULL, a function, or one of 'cramer-von-mises', ",
          "'anderson-darling', 'kolmogorov-smirnov', 'kuiper' and 'mean-residual-error'.",
          "Found: an invalid value for test_statistic."
        ),
        class = c("emp_test_error", "input_argument_error")
      )
    }

    # Set percentiles
    .Object <- .set_percentiles(object = .Object)

    # Set name
    if (is.character(test_statistic)) {
      test_statistic <- gsub("_", "-", test_statistic)
      .Object@name <- paste0(
        "bootstrap numerical goodness-of-fit hypothesis (", test_statistic, ") test"
      )
    } else if (is.null(test_statistic)) {
      .Object@name <- "bootstrap point-wise numerical goodness-of-fit test"

    } else {
      .Object@name <- "bootstrap numerical goodness-of-fit hypothesis test"
    }

    return(.Object)
  }
)



## .set_percentiles ------------------------------------------------------------
setMethod(
  ".set_percentiles",
  signature(object = "empiricalNumericalGOFTest"),
  function(
    object,
    ...
  ) {
    if (object@alternative == "two_sided") {
      # group 1 has either smaller or larger location than group 2
      # by default (0.025, 0.975)
      pctl <- c(0.5 - object@significance_level / 2.0, 0.5 + object@significance_level / 2.0)

    } else if (object@alternative == "less") {
      # group 1 has smaller summary statistic than group 2
      # by default (0.95)
      pctl <- object@significance_level

    } else if (object@alternative == "greater") {
      # group 1 has larger summary statistic than group 2
      # by default (0.05)
      pctl <- 1.0 - object@significance_level

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }

    object@percentiles <- pctl

    return(object)
  }
)


## get_n_draw_summary_samples --------------------------------------------------
setMethod(
  "get_n_draw_summary_samples",
  signature(object = "empiricalNumericalGOFTest"),
  function(object) {
    if (is.null(object@input_data)) return(0L)

    # Unlike estimation, categorical goodness-of-fit tests resample observed
    # data instead of the summary data. Summary data (containing the data under
    # the Null hypothesis) remains fixed.
    return(sum(lengths(object@input_data)))
  }
)



## .sample_data ----------------------------------------------------------------
setMethod(
  ".sample_data",
  signature(object = "empiricalNumericalGOFTest"),
  function(object, random_values, ...) {
    # Produce samples. For numerical goodness-of-fit tests, this
    # is a vector of input observed data with a matched (random) sample of
    # expected.
    data <- object@input_data
    if (is.null(random_values)) {
      ..error_reached_unreachable_code("random_values cannot be NULL as expected is always randomised.")
    }

    # Create vectors of expected.
    n_samples <- sum(lengths(object@input_data))
    if (!is.numeric(object@expected)) {
      ..error_reached_unreachable_code("The expected attribute should either be a function or a numeric vector.")
    }

    # Depending on the contents of expected, we need to sample observed from the
    # expected (empirical distributions) or a quantile function. This then
    # needs to be converted in empirical probabilities to form observed.

    # Expected should also be converted into probabilities, with the same length
    # as observed:
    # - Using the CDF (if stored with the object)
    # - Using the CDF sampled from the quantile function (if the expected attribute is a quantile function)
    # - Using the empirical CDF (if the expected attribute is numeric)

    browser()
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
  signature(object = "empiricalNumericalGOFTest"),
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
  signature(object = "empiricalNumericalGOFTest"),
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



## .set_summary_data -----------------------------------------------------------
setMethod(
  ".set_summary_data",
  signature(object = "empiricalNumericalGOFTest"),
  function(
    object,
    x,
    ...
  ) {
    # Combine summary statistic values into lists.
    x <- unlist(x, recursive = FALSE)
    x <- split(unlist(x), names(x[[1L]]))
    x <- lapply(x, unname)
    x <- lapply(x, sort)

    # Pre-sort the summary data, as the summary data will not be touched.
    object@summary_data <- x

    return(object)
  }
)



## ..bootstrap_test ------------------------------------------------------------
setMethod(
  "..bootstrap_test",
  signature(object = "empiricalNumericalGOFTest"),
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
  signature(object = "empiricalNumericalGOFTest"),
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

    # Iterate for each variable and "all".
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
        hypothesis_test_only = hypothesis_test_only,
        cache_name = group
      )
      test_output[[group]] <- results
    }

    return(test_output)
  }
)


## ..test ----------------------------------------------------------------------
setMethod(
  "..test",
  signature(object = "empiricalNumericalGOFTest"),
  function(
    object,
    x_0,
    x_1,
    hypothesis_test_only = FALSE,
    cache_name = NULL,
    ...
  ) {
    if (is.null(cache_name)) ..error_reached_unreachable_code("cache_name cannot be missing")

    # Use ..test_difference for consistency.
    results <- ..test_difference(
      object = object,
      x = x_1,
      location = x_0,
      hypothesis_test_only = hypothesis_test_only,
      cache_name = cache_name
    )

    if (hypothesis_test_only) {
      return(list("reject_null" = results$reject_null))
    }

    # Add additional data.
    results <- c(
      results,
      list(
        "expected" = object@expected,
        "test_statistic_observed" = x_0,
        "test_statistic_critical" = max(results$confidence_interval)
      )
    )

    return(results)
  }
)
