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
#'   is then sampled to identify the expected distribution. The **first**
#'   argument is expected to be a vector of quantile probabilies, (e.g., `p` of
#'   `stats::qnorm`).
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
