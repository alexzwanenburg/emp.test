testthat::test_that(
  "Equivalence test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Two one-sided test procedure.
    ## Compute pooled standard deviation.
    sd_pooled <- stats::sd(x_1)

    ## Degrees of freedom
    df <- length(x_1) - 1L

    ## t-statistics
    t_lower <- (mean(x_1) - (-0.2)) / (sd_pooled / sqrt(length(x_1)))
    t_upper <- (mean(x_1) - (0.2)) / (sd_pooled / sqrt(length(x_1)))

    ## p-values
    p_lower <- stats::pt(t_lower, df, lower.tail = FALSE)
    p_upper <- stats::pt(t_upper, df, lower.tail = TRUE)
    p_value <- max(p_lower, p_upper)

    # Comparison with two one-sided test procedure.
    results_1 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = 0.2
    )

    testthat::expect_lte(abs(results_1$x_1$p_value - p_value), 0.01)
    testthat::expect_lte(p_value, 0.29)
    testthat::expect_gte(p_value, 0.28)

    # Larger tolerance --> lower p-value
    results_2 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = 0.3
    )
    testthat::expect_gt(results_1$x_1$p_value, results_2$x_1$p_value)

    # With two tolerances.
    results_3 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = c(-0.3, 0.2)
    )
    testthat::expect_equal(results_1$x_1$p_value, results_3$x_1$p_value)

    # Tolerance using a function -- in this case the same tolerance as above.
    tol_fun <- function(x1) (return(c(-0.3, 0.2)))
    results_4 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = tol_fun
    )
    testthat::expect_equal(results_1$x_1$p_value, results_4$x_1$p_value)

    # Tolerance using a function with extra arguments.
    tol_fun <- function(x1, flag) (if (flag) return(c(-0.3, 0.2)) else return(c(-0.1, 0.1)))
    results_5 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = TRUE)
    )
    testthat::expect_equal(results_1$x_1$p_value, results_5$x_1$p_value)

    results_6 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = FALSE)
    )
    testthat::expect_gt(results_6$x_1$p_value, results_5$x_1$p_value)
  }
)


testthat::test_that(
  "Non-inferiority test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    sd_pooled <- stats::sd(x_1)
    df <- length(x_1) - 1L
    t_lower <- (mean(x_1) - (-0.05)) / (sd_pooled / sqrt(length(x_1)))
    p_value <- stats::pt(t_lower, df, lower.tail = FALSE)

    # Comparison with two one-sided test procedure.
    results_1 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = -0.05,
      alternative = "not_inferior"
    )

    testthat::expect_lte(abs(results_1$x_1$p_value - p_value), 0.01)
    testthat::expect_lte(results_1$x_1$p_value, 0.05)
    testthat::expect_gte(results_1$x_1$p_value, 0.04)

    # Larger tolerance --> lower p-value
    results_2 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = -0.1,
      alternative = "not_inferior"
    )
    testthat::expect_gt(results_1$x_1$p_value, results_2$x_1$p_value)

    # With two tolerances (technically not ok)
    results_3 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = c(-0.05, 0.3),
      alternative = "not_inferior"
    )
    testthat::expect_equal(results_1$x_1$p_value, results_3$x_1$p_value)

    # Tolerance using a function -- in this case the same tolerance as above.
    tol_fun <- function(x1) (return(-0.05))
    results_4 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = tol_fun,
      alternative = "not_inferior"
    )
    testthat::expect_equal(results_1$x_1$p_value, results_4$x_1$p_value)

    # Tolerance using a function with extra arguments.
    tol_fun <- function(x1, flag) (if (flag) return(-0.05) else return(0.1))
    results_5 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = TRUE),
      alternative = "not_inferior"
    )
    testthat::expect_equal(results_1$x_1$p_value, results_5$x_1$p_value)

    results_6 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = FALSE),
      alternative = "not_inferior"
    )
    testthat::expect_gt(results_6$x_1$p_value, results_5$x_1$p_value)
  }
)


testthat::test_that(
  "Non-superiority test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Comparison with two one-sided test procedure.
    results_1 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = 0.2,
      alternative = "not_superior"
    )

    testthat::expect_lte(results_1$x_1$p_value, 0.28)
    testthat::expect_gte(results_1$x_1$p_value, 0.27)

    # Larger tolerance --> lower p-value
    results_2 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = 0.3,
      alternative = "not_superior"
    )
    testthat::expect_gt(results_1$x_1$p_value, results_2$x_1$p_value)

    # With two tolerances (technically not ok)
    results_3 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = c(-0.2, 0.2),
      alternative = "not_superior"
    )
    testthat::expect_equal(results_1$x_1$p_value, results_3$x_1$p_value)

    # Tolerance using a function -- in this case the same tolerance as above.
    tol_fun <- function(x1) (return(0.2))
    results_4 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = tol_fun,
      alternative = "not_superior"
    )
    testthat::expect_equal(results_1$x_1$p_value, results_4$x_1$p_value)

    # Tolerance using a function with extra arguments.
    tol_fun <- function(x1, flag) (if (flag) return(0.2) else return(0.1))
    results_5 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = TRUE),
      alternative = "not_superior"
    )
    testthat::expect_equal(results_1$x_1$p_value, results_5$x_1$p_value)

    results_6 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = FALSE),
      alternative = "not_superior"
    )
    testthat::expect_gt(results_6$x_1$p_value, results_5$x_1$p_value)
  }
)



testthat::test_that(
  "Equivalence test functions correctly with more than 2 variables",
  {
    .two_one_sided_test <- function(x_1) {
      sd_pooled <- stats::sd(x_1)
      df <- length(x_1) - 1L
      t_lower <- (mean(x_1) - (-0.2)) / (sd_pooled / sqrt(length(x_1)))
      t_upper <- (mean(x_1) - (0.2)) / (sd_pooled / sqrt(length(x_1)))
      p_lower <- stats::pt(t_lower, df, lower.tail = FALSE)
      p_upper <- stats::pt(t_upper, df, lower.tail = TRUE)
      p_value <- max(p_lower, p_upper)

      return(p_value)
    }

    x_1 <- emp.test:::.rnorm(50L, seed = 19L)
    x_2 <- 1.1 * x_1
    x_3 <- 1.5 * x_1

    # Comparison with two one-sided test procedure.
    results_1 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      x_3 = x_3,
      tolerance = 0.2
    )

    results_2a <- .two_one_sided_test(x_1)
    results_2b <- .two_one_sided_test(x_2)
    results_2c <- .two_one_sided_test(x_3)

    testthat::expect_lte(abs(results_1$x_1$p_value - results_2a), 0.01)
    testthat::expect_lte(results_2a, 0.29)
    testthat::expect_gte(results_2a, 0.28)

    testthat::expect_lte(abs(results_1$x_2$p_value - results_2b), 0.03)
    testthat::expect_lte(results_2b, 0.35)
    testthat::expect_gte(results_2b, 0.34)

    testthat::expect_lte(abs(results_1$x_3$p_value - results_2c), 0.03)
    testthat::expect_lte(results_2c, 0.52)
    testthat::expect_gte(results_2c, 0.51)
  }
)



# Test with different summary statistic.
testthat::test_that(
  "Equivalence test functions correctly with different summary function",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Standard deviation.
    results_1 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = c(0.65, 0.85),
      summary_function = stats::sd
    )
    testthat::expect_lte(results_1$x_1$p_value, 0.12)
    testthat::expect_gte(results_1$x_1$p_value, 0.11)

    # Percentile.
    results_2 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = c(0.6, 0.8),
      summary_function = stats::quantile,
      summary_function_args = list("probs" = 0.75)
    )
    testthat::expect_lte(results_2$x_1$p_value, 0.37)
    testthat::expect_gte(results_2$x_1$p_value, 0.36)
  }
)



# Test without summary statistic.
testthat::test_that(
  "One sample equivalence test functions correctly without a summary statistic.",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Without summary function.
    results_1 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = 0.20,
      summary_function = NULL
    )
    results_2 <- emp.test::one_sample_equivalence_test(
      x_1 = x_1,
      tolerance = 0.20
    )
    testthat::expect_lte(results_1$`x_2 - x_1`$p_value, 0.45)
    testthat::expect_gte(results_1$`x_2 - x_1`$p_value, 0.43)

    # The direct difference should have a higher p-value than the difference of
    # the means.
    testthat::expect_gt(results_1$`x_2 - x_1`$p_value, results_2$`x_2 - x_1`$p_value)
  }
)
