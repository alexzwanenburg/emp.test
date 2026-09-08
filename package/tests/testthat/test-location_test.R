
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
    testthat::expect_lte(results_2$p.value, 0.55)
    testthat::expect_gte(results_2$p.value, 0.50)

    # No difference.
    results_1 <- emp.test::location_test(
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
    results_1 <- emp.test::location_test(
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
