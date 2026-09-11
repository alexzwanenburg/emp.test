
testthat::test_that(
  "Two-sided two-sample test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Small difference.
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 0.1
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 + 0.1
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_lte(results_2$p.value, 0.55)
    testthat::expect_gte(results_2$p.value, 0.50)

    # No difference.
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_equal(results_2$p.value, 1.00)

    # Medium difference
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 0.4
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 + 0.4
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_lte(results_2$p.value, 0.05)

    # Large difference
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 1.0
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 + 1.0
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.01)
    testthat::expect_lte(results_2$p.value, 0.001)
  }
)



testthat::test_that(
  "Lower-tail two-sample test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Small difference.
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 0.1,
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 + 0.1,
      alternative = "less"
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_lte(results_2$p.value, 0.27)
    testthat::expect_gte(results_2$p.value, 0.25)

    # No difference.
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1,
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1,
      alternative = "less"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_equal(results_2$p.value, 0.50)

    # Medium difference
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 0.4,
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 + 0.4,
      alternative = "less"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_lte(results_2$p.value, 0.05)

    # Medium negative difference
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 - 0.4,
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 - 0.4,
      alternative = "less"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_gte(results_2$p.value, 0.95)

    # Large difference
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 1.0,
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 + 1.0,
      alternative = "less"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.01)
    testthat::expect_lte(results_2$p.value, 0.001)
  }
)



testthat::test_that(
  "Upper-tail two-sample test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Small difference.
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 - 0.1,
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 - 0.1,
      alternative = "greater"
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_lte(results_2$p.value, 0.28)
    testthat::expect_gte(results_2$p.value, 0.25)

    # No difference.
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1,
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1,
      alternative = "greater"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_equal(results_2$p.value, 0.50)

    # Medium difference
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 - 0.4,
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 - 0.4,
      alternative = "greater"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_lte(results_2$p.value, 0.05)

    # Medium negative difference
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 0.4,
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 + 0.4,
      alternative = "greater"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_gte(results_2$p.value, 0.95)

    # Large difference
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 - 1.0,
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1 - 1.0,
      alternative = "greater"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.01)
    testthat::expect_lte(results_2$p.value, 0.001)
  }
)


# Test on multiple groups.
testthat::test_that(
  "Two-sample difference test functions correctly with multiple groups",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 0.2,
      x_3 = x_1 + 0.6
    )
    results_2a <- stats::t.test(
      x = x_1,
      y = x_1 + 0.2
    )
    results_2b <- stats::t.test(
      x = x_1,
      y = x_1 + 0.6
    )
    results_2c <- stats::t.test(
      x = x_1 + 0.2,
      y = x_1 + 0.6
    )
    testthat::expect_length(results_1$`x_1 - x_2`$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$`x_1 - x_2`$p_value - results_2a$p.value), 0.05)
    testthat::expect_lte(results_2a$p.value, 0.25)
    testthat::expect_gte(results_2a$p.value, 0.20)

    testthat::expect_length(results_1$`x_1 - x_3`$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$`x_1 - x_3`$p_value - results_2b$p.value), 0.05)
    testthat::expect_lte(results_2b$p.value, 0.001)

    testthat::expect_length(results_1$`x_2 - x_3`$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$`x_2 - x_3`$p_value - results_2b$p.value), 0.05)
    testthat::expect_lte(results_2b$p.value, 0.001)
  }
)


# Test with different summary statistic.
testthat::test_that(
  "Two-sided two-sample test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Standard deviation.
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 0.1,
      summary_function = stats::sd
    )
    testthat::expect_length(results_1$`x_1 - x_2`$confidence_interval, 2L)
    testthat::expect_gte(results_1$`x_1 - x_2`$p_value, 0.95)

    # Percentile.
    results_2 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1 + 0.1,
      summary_function = stats::quantile,
      summary_function_args = list("probs" = 0.75)
    )
    testthat::expect_length(results_2$`x_1 - x_2`$confidence_interval, 2L)
    testthat::expect_lte(results_2$`x_1 - x_2`$p_value, 0.65)
    testthat::expect_gte(results_2$`x_1 - x_2`$p_value, 0.60)
  }
)
