
testthat::test_that(
  "Two-sided paired two-sample test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # With difference.
    x_2 <- x_1 + 0.1 + emp.test:::.rnorm(50L, sd = 0.2, seed = 20L)
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_2,
      paired = TRUE
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_2,
      paired = TRUE
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_lte(results_2$p.value, 0.010)
    testthat::expect_gte(results_2$p.value, 0.005)

    # No difference.
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_1,
      paired = TRUE
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_1,
      paired = TRUE
    )
    testthat::expect_true(is.na(results_1$`x_1 - x_2`$p_value[[1L]]))
    testthat::expect_true(is.na(results_2$p.value))
  }
)



testthat::test_that(
  "Lower-tail paired two-sample test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # With positive difference.
    x_2 <- x_1 + 0.1 + emp.test:::.rnorm(50L, sd = 0.2, seed = 20L)
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_2,
      alternative = "less",
      paired = TRUE
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_2,
      alternative = "less",
      paired = TRUE
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_lte(results_2$p.value, 0.005)
    testthat::expect_gte(results_2$p.value, 0.001)

    # With negative difference
    x_2 <- x_1 - 0.1 + emp.test:::.rnorm(50L, sd = 0.2, seed = 20L)
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_2,
      alternative = "less",
      paired = TRUE
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_2,
      alternative = "less",
      paired = TRUE
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_gte(results_2$p.value, 0.99)
  }
)



testthat::test_that(
  "Upper-tail two-sample test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # With positive difference.
    x_2 <- x_1 + 0.1 + emp.test:::.rnorm(50L, sd = 0.2, seed = 20L)
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_2,
      alternative = "greater",
      paired = TRUE
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_2,
      alternative = "greater",
      paired = TRUE
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_gte(results_2$p.value, 0.99)

    # With negative difference
    x_2 <- x_1 - 0.1 + emp.test:::.rnorm(50L, sd = 0.2, seed = 20L)
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_2,
      alternative = "greater",
      paired = TRUE
    )
    results_2 <- stats::t.test(
      x = x_1,
      y = x_2,
      alternative = "greater",
      paired = TRUE
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.001)
    testthat::expect_lte(results_2$p.value, 0.001)
  }
)


# Test on multiple groups.
testthat::test_that(
  "Two-sample difference test functions correctly with multiple groups",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)
    x_2 <- x_1 + 0.1 + emp.test:::.rnorm(50L, sd = 0.2, seed = 20L)
    x_3 <- x_1 + 0.05 + emp.test:::.rnorm(50L, sd = 0.2, seed = 21L)
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_2,
      x_3 = x_3,
      paired = TRUE
    )
    results_2a <- stats::t.test(
      x = x_1,
      y = x_2,
      paired = TRUE
    )
    results_2b <- stats::t.test(
      x = x_1,
      y = x_3,
      paired = TRUE
    )
    results_2c <- stats::t.test(
      x = x_2,
      y = x_3,
      paired = TRUE
    )
    testthat::expect_length(results_1$`x_1 - x_2`$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$`x_1 - x_2`$p_value - results_2a$p.value), 0.01)
    testthat::expect_lte(results_2a$p.value, 0.01)

    testthat::expect_length(results_1$`x_1 - x_3`$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$`x_1 - x_3`$p_value - results_2b$p.value), 0.01)
    testthat::expect_lte(results_2b$p.value, 0.01)
    testthat::expect_lte(results_2a$p.value, results_2b$p.value)  # The difference between x_1 and x_2 is larger than between x_1 and x_3

    testthat::expect_length(results_1$`x_2 - x_3`$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$`x_2 - x_3`$p_value - results_2c$p.value), 0.05)
    testthat::expect_lte(results_2c$p.value, 0.90)
    testthat::expect_gte(results_2c$p.value, 0.85)
  }
)


# Test with different summary statistic.
testthat::test_that(
  "Two-sided two-sample test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)
    x_2 <- emp.test:::.rnorm(50L, sd = 0.2, seed = 20L)

    # Standard deviation.
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_2,
      summary_function = stats::sd,
      paired = TRUE
    )
    testthat::expect_length(results_1$`x_1 - x_2`$confidence_interval, 2L)
    testthat::expect_lte(results_1$`x_1 - x_2`$p_value, 0.01)

    # Percentile.
    results_2 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_2,
      summary_function = stats::quantile,
      summary_function_args = list("probs" = 0.75)
    )
    testthat::expect_length(results_2$`x_1 - x_2`$confidence_interval, 2L)
    testthat::expect_lte(results_1$`x_1 - x_2`$p_value, 0.01)
  }
)


# Test without summary statistic.
testthat::test_that(
  "Two-sided two-sample test functions correctly without summary function",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)
    x_2 <- x_1 + 0.1 + emp.test:::.rnorm(50L, sd = 0.2, seed = 20L)

    # Set summary_function = NULL to actually
    results_1 <- emp.test::difference_test(
      x_1 = x_1,
      x_2 = x_2,
      summary_function = NULL,
      paired = TRUE
    )

    mu <- mean(x_1 - x_2)
    sigma <- stats::sd(x_1 - x_2)
    t <- (mu) / sigma
    p <- 2.0 * stats::pt(q = -abs(t), df = length(x_1) - 1L)

    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$x_1$p_value - p), 0.15)

        # Difference is larger than for tests using sample means, since x_1 and x_2 are not fully normally distributed.
    testthat::expect_lte(results_1$x_1$p_value, 0.65)
    testthat::expect_gte(results_1$x_1$p_value, 0.55)
  }
)
