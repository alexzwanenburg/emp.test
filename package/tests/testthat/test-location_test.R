
testthat::test_that(
  "Two-sided location test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Small difference.
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 0.1
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = 0.1
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_lte(results_2$p.value, 0.75)
    testthat::expect_gte(results_2$p.value, 0.70)

    # No difference.
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = mean(x_1)
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = mean(x_1)
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_equal(results_2$p.value, 1.00)

    # Medium difference
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 0.4
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = 0.4
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_lte(results_2$p.value, 0.05)

    # Large difference
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 1.0
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = 1.0
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.01)
    testthat::expect_lte(results_2$p.value, 0.001)
  }
)



testthat::test_that(
  "Lower-tail one-sample location test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Small difference.
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 0.1,
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = 0.1,
      alternative = "less"
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_lte(results_2$p.value, 0.65)
    testthat::expect_gte(results_2$p.value, 0.60)

    # No difference.
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = mean(x_1),
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = mean(x_1),
      alternative = "less"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_equal(results_2$p.value, 0.50)

    # Medium difference
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 0.4,
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = 0.4,
      alternative = "less"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_lte(results_2$p.value, 0.05)

    # Medium negative difference
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = -0.4,
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = -0.4,
      alternative = "less"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_gte(results_2$p.value, 0.95)

    # Large difference
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 1.0,
      alternative = "less"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = 1.0,
      alternative = "less"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.01)
    testthat::expect_lte(results_2$p.value, 0.001)
  }
)



testthat::test_that(
  "Upper-tail one-sample location test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Small difference.
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 0.1,
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = 0.1,
      alternative = "greater"
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_lte(results_2$p.value, 0.40)
    testthat::expect_gte(results_2$p.value, 0.35)

    # No difference.
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = mean(x_1),
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = mean(x_1),
      alternative = "greater"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.05)
    testthat::expect_equal(results_2$p.value, 0.50)

    # Medium difference
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = -0.1,
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = -0.1,
      alternative = "greater"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_lte(results_2$p.value, 0.05)

    # Medium negative difference
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 0.4,
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = 0.4,
      alternative = "greater"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.02)
    testthat::expect_gte(results_2$p.value, 0.95)

    # Large difference
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = -1.0,
      alternative = "greater"
    )
    results_2 <- stats::t.test(
      x = x_1,
      mu = -1.0,
      alternative = "greater"
    )
    testthat::expect_lte(abs(results_1[[1L]]$p_value - results_2$p.value), 0.01)
    testthat::expect_lte(results_2$p.value, 0.001)
  }
)


# Test on multiple groups.
testthat::test_that(
  "Location test functions correctly with multiple groups",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)
    x_2 <- emp.test:::.rnorm(50L, seed = 20L)

    # Small difference.
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      x_2 = x_2,
      location = 0.1
    )
    results_2a <- stats::t.test(
      x = x_1,
      mu = 0.1
    )
    results_2b <- stats::t.test(
      x = x_2,
      mu = 0.1
    )
    testthat::expect_length(results_1$x_1$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$x_1$p_value - results_2a$p.value), 0.05)
    testthat::expect_lte(results_2a$p.value, 0.75)
    testthat::expect_gte(results_2a$p.value, 0.70)

    testthat::expect_length(results_1$x_2$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$x_2$p_value - results_2b$p.value), 0.05)
    testthat::expect_lte(results_2b$p.value, 0.25)
    testthat::expect_gte(results_2b$p.value, 0.20)
  }
)

# Test with different summary statistic.
testthat::test_that(
  "Two-sided location test functions correctly with different summary functions",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Small difference -- standard deviation for the current seed is about 0.80.
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 0.8,
      summary_function = stats::sd
    )
    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(results_1$x_1$p_value, 0.80)
    testthat::expect_gte(results_1$x_1$p_value, 0.75)

    # Percentile.
    results_2 <- emp.test::location_test(
      x_1 = x_1,
      location = 0.9,
      summary_function = stats::quantile,
      summary_function_args = list("probs" = 0.75)
    )
    testthat::expect_length(results_2$x_1$confidence_interval, 2L)
    testthat::expect_lte(results_2$x_1$p_value, 0.20)
    testthat::expect_gte(results_2$x_1$p_value, 0.15)
  }
)


# Test without summary statistic.
testthat::test_that(
  "Two-sided location test functions correctly without summary function",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)

    # Set summary_function = NULL to actually
    results_1 <- emp.test::location_test(
      x_1 = x_1,
      location = 0.4,
      summary_function = NULL
    )

    mu <- mean(x_1)
    sigma <- stats::sd(x_1)
    t <- (mu - 0.4) / sigma
    p <- 2.0 * stats::pt(q = -abs(t), df = length(x_1) - 1L)

    testthat::expect_length(results_1[[1L]]$confidence_interval, 2L)
    testthat::expect_lte(abs(results_1$x_1$p_value - p), 0.15)
    # Difference is larger than for tests using sample means, since x_1 is not fully normally distributed.
    testthat::expect_lte(results_1$x_1$p_value, 0.85)
    testthat::expect_gte(results_1$x_1$p_value, 0.75)
  }
)
