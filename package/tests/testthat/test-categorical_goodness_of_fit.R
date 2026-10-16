testthat::skip()

testthat::test_that(
  "Categorical goodness of fit test functions correctly",
  {
    x_1 <- emp.test:::.rcat(50L, p_categories = c(1/3, 1/3, 1/3), seed = 19L)
    x_2 <- emp.test:::.rcat(45L, p_categories = c(0.10, 0.8, 0.10), seed = 20L)

    x_1 <- emp.test:::.convert_vector_to_counts(x_1, categories = paste0("c_", seq_len(3L)))
    x_2 <- emp.test:::.convert_vector_to_counts(x_2, categories = paste0("c_", seq_len(3L)))

    # Test with two different samples against the joint distribution.
    results_1 <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      x_2,
      test_statistic = "chi_square"
    )
    results_2 <- stats::chisq.test(rbind(x_1, x_2))

    testthat::expect_length(results_1, 3L)
    testthat::expect_setequal(names(results_1), c("x_1", "x_2", "all"))
    testthat::expect_lte(abs(results_1$all$p_value - results_2$p.value), 0.02)
    testthat::expect_lte(results_2$p.value, 0.05)

    # Test with one sample.
    results_1 <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      expected = c(0.25, 0.5, 0.25),
      test_statistic = "chi_square"
    )
    results_2 <- stats::chisq.test(x_1, p = c(0.25, 0.5, 0.25))

    testthat::expect_length(results_1, 1L)
    testthat::expect_setequal(names(results_1), c("x_1"))
    testthat::expect_lte(abs(results_1$x_1$p_value - results_2$p.value), 0.02)
    testthat::expect_lte(results_2$p.value, 0.09)
    testthat::expect_gte(results_2$p.value, 0.08)

    # Test one sample against another sample.
    # Note: no direct equivalent for stats::chisq.test.
    x_3 <- emp.test:::.rcat(55L, p_categories = c(0.25, 0.5, 0.25), seed = 20L)
    x_3 <- emp.test:::.convert_vector_to_counts(x_3, categories = paste0("c_", seq_len(3L)))

    results_1b <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      expected = x_3,
      test_statistic = "chi_square"
    )
    testthat::expect_length(results_1b, 1L)
    testthat::expect_setequal(names(results_1), c("x_1"))
    testthat::expect_lte(abs(results_1b$x_1$p_value - results_1$x_1$p_value), 0.10)
    testthat::expect_lte(results_1b$p.value, 0.02)
    testthat::expect_gte(results_1b$p.value, 0.01)



    # Test using g-test.


    # Test using cressie_read.


    # Test using expected as a function.


    # Test using observed as a sequence of characters.


    # Test using observed as a sequence of factors.


    # Test using observed as a sequence of logical.


    # Test using custom test-statistic.


    # Test without test-statistic.

  }
)
