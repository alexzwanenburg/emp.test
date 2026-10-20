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
      test_statistic = "chi_squared"
    )
    results_2 <- stats::chisq.test(rbind(x_1, x_2))

    testthat::expect_length(results_1, 3L)
    testthat::expect_setequal(names(results_1), c("x_1", "x_2", "all"))
    testthat::expect_lte(abs(results_1$all$p_value - results_2$p.value), 0.02)
    testthat::expect_lte(results_2$p.value, 0.05)

    # Observed are characters.
    x_1c <- c("alpha", "beta", "gamma")[emp.test:::.rcat(50L, p_categories = c(1/3, 1/3, 1/3), seed = 19L)]
    x_2c <- c("alpha", "beta", "gamma")[emp.test:::.rcat(45L, p_categories = c(0.10, 0.8, 0.10), seed = 20L)]
    results_1char <- emp.test::categorical_goodness_of_fit_test(
      x_1c,
      x_2c,
      test_statistic = "chi_squared"
    )
    testthat::expect_equal(results_1$x_1$p_value, results_1char$x_1$p_value)
    testthat::expect_equal(results_1$x_2$p_value, results_1char$x_2$p_value)
    testthat::expect_equal(results_1$all$p_value, results_1char$all$p_value)

    # Observed are categorical variables.
    x_1f <- factor(x_1c, levels = c("alpha", "beta", "gamma"))
    x_2f <- factor(x_2c, levels = c("alpha", "beta", "gamma"))
    results_1fact <- emp.test::categorical_goodness_of_fit_test(
      x_1f,
      x_2f,
      test_statistic = "chi_squared"
    )
    testthat::expect_equal(results_1$x_1$p_value, results_1fact$x_1$p_value)
    testthat::expect_equal(results_1$x_2$p_value, results_1fact$x_2$p_value)
    testthat::expect_equal(results_1$all$p_value, results_1fact$all$p_value)

    # Observed are logical variables.
    x_1l <- x_1c == "beta"
    x_2l <- x_2c == "beta"
    results_1fact <- emp.test::categorical_goodness_of_fit_test(
      x_1l,
      x_2l,
      test_statistic = "chi_squared"
    )
    results_2fact <- stats::chisq.test(rbind(
      emp.test:::.convert_vector_to_counts(x_1l, categories = c(FALSE, TRUE)),
      emp.test:::.convert_vector_to_counts(x_2l, categories = c(FALSE, TRUE))
    ))
    testthat::expect_length(results_1fact, 3L)
    testthat::expect_setequal(names(results_1fact), c("x_1", "x_2", "all"))
    testthat::expect_lte(abs(results_1fact$all$p_value - results_2fact$p.value), 0.01)
    testthat::expect_lte(results_2fact$p.value, 0.01)

    # Test with one sample.
    results_1 <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      expected = c(0.25, 0.5, 0.25),
      test_statistic = "chi_squared"
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
      test_statistic = "chi_squared"
    )
    testthat::expect_length(results_1b, 1L)
    testthat::expect_setequal(names(results_1b), c("x_1"))
    testthat::expect_lte(abs(results_1b$x_1$p_value - results_1$x_1$p_value), 0.10)
    testthat::expect_lte(results_1b$x_1$p_value, 0.02)
    testthat::expect_gte(results_1b$x_1$p_value, 0.01)

    # Test using g-test.
    results_1c <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      expected = x_3,
      test_statistic = "g"
    )
    testthat::expect_length(results_1c, 1L)
    testthat::expect_setequal(names(results_1c), c("x_1"))
    testthat::expect_lte(results_1c$x_1$p_value, 0.03)
    testthat::expect_gte(results_1c$x_1$p_value, 0.02)

    # Test using cressie_read.
    results_1d <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      expected = x_3,
      test_statistic = "cressie_read"
    )
    testthat::expect_length(results_1d, 1L)
    testthat::expect_setequal(names(results_1d), c("x_1"))
    testthat::expect_lte(results_1d$x_1$p_value, 0.02)
    testthat::expect_gte(results_1d$x_1$p_value, 0.01)

    # Test using cressie_read and custom lambda.
    results_1e <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      expected = x_3,
      test_statistic = "cressie_read",
      test_statistic_args = list("lambda" = 0.0)  # equivalent to g-test
    )
    testthat::expect_length(results_1e, 1L)
    testthat::expect_setequal(names(results_1e), c("x_1"))
    testthat::expect_equal(results_1c$x_1$p_value, results_1e$x_1$p_value)

    # Test using custom test-statistic.
    results_1f <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      expected = x_3,
      test_statistic = function(x, y) (sum(abs(x-y) / y))
    )
    testthat::expect_length(results_1f, 1L)
    testthat::expect_setequal(names(results_1f), c("x_1"))
    testthat::expect_lte(results_1f$x_1$p_value, 0.05)
    testthat::expect_gte(results_1f$x_1$p_value, 0.04)

    # Test using custom test-statistic and custom arguments.
    results_1g <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      expected = x_3,
      test_statistic = function(x, y, beta) (beta + sum(abs(x-y) / y)),
      test_statistic_args = list("beta" = 5.0)
    )
    testthat::expect_length(results_1g, 1L)
    testthat::expect_setequal(names(results_1g), c("x_1"))
    testthat::expect_equal(results_1g$x_1$p_value, results_1g$x_1$p_value)
    testthat::expect_equal(results_1f$x_1$test_statistic_critical + 5.0, results_1g$x_1$test_statistic_critical)

    # Test using expected as a function.
    results_1h <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      expected = emp.test:::.rcat,
      expected_args = list(p_categories = c(0.25, 0.5, 0.25), seed = 20L)  # Same distribution as x_3.
    )
    testthat::expect_length(results_1h, 1L)
    testthat::expect_setequal(names(results_1h), c("x_1"))
    testthat::expect_equivalent(round(results_1h$x_1$expected, digits = 2L), c(0.25, 0.5, 0.25))
    testthat::expect_lte(results_1h$x_1$p_value, 0.09)
    testthat::expect_gte(results_1h$x_1$p_value, 0.08)

    # Test using expected as a function that produces a sequence of characters.
    x_1c <- x_1
    names(x_1c) <- c("alpha", "beta", "gamma")

    results_1k <- emp.test::categorical_goodness_of_fit_test(
      x_1c,
      expected = function(n, p_categories, seed) {
        x <- c("alpha", "beta", "gamma")
        ii <- emp.test:::.rcat(n, p_categories = p_categories, seed = seed)
        return(x[ii])
      },
      expected_args = list(p_categories = c(0.25, 0.5, 0.25), seed = 20L)  # Same distribution as x_3.
    )
    testthat::expect_length(results_1k, 1L)
    testthat::expect_setequal(names(results_1k), c("x_1"))
    testthat::expect_equivalent(round(results_1k$x_1$expected, digits = 2L), c(0.25, 0.5, 0.25))
    testthat::expect_equal(results_1h$x_1$p_value, results_1k$x_1$p_value)

    # Test using expected as a function that produces a sequence of categories
    results_1m <- emp.test::categorical_goodness_of_fit_test(
      x_1c,
      expected = function(n, p_categories, seed) {
        x <- factor(c("alpha", "beta", "gamma"))
        ii <- emp.test:::.rcat(n, p_categories = p_categories, seed = seed)
        return(x[ii])
      },
      expected_args = list(p_categories = c(0.25, 0.5, 0.25), seed = 20L)  # Same distribution as x_3.
    )
    testthat::expect_length(results_1m, 1L)
    testthat::expect_setequal(names(results_1m), c("x_1"))
    testthat::expect_equivalent(round(results_1m$x_1$expected, digits = 2L), c(0.25, 0.5, 0.25))
    testthat::expect_equal(results_1h$x_1$p_value, results_1m$x_1$p_value)

    # Test using expected as a function that produces a sequence of logical
    # variables.
    x_1l <- emp.test:::.rcat(50L, p_categories = c(1/2, 1/2), seed = 19L)
    x_1l <- x_1l == 2L

    results_1n <- emp.test::categorical_goodness_of_fit_test(
      x_1l,
      expected = function(n, p_categories, seed) {
        x <- c(FALSE, TRUE)
        ii <- emp.test:::.rcat(n, p_categories = p_categories, seed = seed)
        return(x[ii])
      },
      expected_args = list(p_categories = c(0.25, 0.75), seed = 20L)  # Same distribution as x_3.
    )
    testthat::expect_length(results_1n, 1L)
    testthat::expect_setequal(names(results_1n), c("x_1"))
    testthat::expect_equivalent(round(results_1n$x_1$expected, digits = 2L), c(0.25, 0.75))
  }
)



testthat::test_that(
  "Point-wise categorical goodness of fit test functions correctly",
  {
    x_1 <- emp.test:::.rcat(50L, p_categories = c(1/3, 1/3, 1/3), seed = 19L)
    x_2 <- emp.test:::.rcat(45L, p_categories = c(0.10, 0.8, 0.10), seed = 20L)

    x_1 <- emp.test:::.convert_vector_to_counts(x_1, categories = paste0("c_", seq_len(3L)))
    x_2 <- emp.test:::.convert_vector_to_counts(x_2, categories = paste0("c_", seq_len(3L)))

    # Test with two different samples against the joint distribution.
    results_1 <- emp.test::categorical_goodness_of_fit_test(
      x_1,
      x_2,
      test_statistic = NULL
    )
    testthat::expect_length(results_1, 2L)
    testthat::expect_setequal(names(results_1), c("x_1", "x_2"))
    testthat::expect_lte(results_1$x_1$p_value, 0.04)
    testthat::expect_gte(results_1$x_1$p_value, 0.03)
    testthat::expect_lte(results_1$x_2$p_value, 0.02)
    testthat::expect_gte(results_1$x_2$p_value, 0.01)
  }
)
