testthat::test_that(
  "Categorical goodness of fit test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, mean = 1.0, sd = 1.0, seed = 19L)
    x_2 <- emp.test:::.rnorm(45L, mean = 0.8, sd = 1.2, seed = 20L)

    # Test with two different samples against the joint distribution.
    results_1 <- emp.test::numerical_goodness_of_fit_test(
      x_1,
      x_2,
      test_statistic = "kolmogorov-smirnov"
    )
    results_2 <- stats::ks.test(x_1, x_2)

    # testthat::expect_length(results_1, 3L)
    # testthat::expect_setequal(names(results_1), c("x_1", "x_2", "all"))
    # testthat::expect_lte(abs(results_1$all$p_value - results_2$p.value), 0.02)
    # testthat::expect_lte(results_2$p.value, 0.05)
  }
)
