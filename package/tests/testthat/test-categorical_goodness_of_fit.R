testthat::test_that(
  "Categorical goodness of test functions correctly",
  {
    x_1 <- emp.test:::.rcat(50L, p_categories = c(1/3, 1/3, 1/3), seed = 19L)
    x_2 <- emp.test:::.rcat(45L, p_categories = c(0.25, 0.5, 0.25), seed = 20L)

    x_1 <- emp.test:::.convert_vector_to_counts(x_1, categories = paste0("x_", seq_len(3L)))
    x_2 <- emp.test:::.convert_vector_to_counts(x_2, categories = paste0("x_", seq_len(3L)))

    results_1 <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      x_2,
      test_statistic = "chi_square"
    )
    results_2 <- stats::chisq.test(rbind(x_1, x_2))

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
