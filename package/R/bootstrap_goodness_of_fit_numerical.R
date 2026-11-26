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




# empiricalCategoricalGOFTest object -------------------------------------------
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
    .Object@input_data <- format_data_categorical(data = data)

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
