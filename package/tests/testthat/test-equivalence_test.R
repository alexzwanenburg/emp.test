testthat::test_that(
  "Equivalence test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)
    x_2 <- x_1

    # Small tolerance.
    results_1 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = 0.2
    )

    # Two one-sided test procedure.
    ## Compute pooled standard deviation.
    sd_pooled <- sqrt((
        (length(x_1) - 1L) * stats::sd(x_1)^2.0 +
        (length(x_2) - 1L) * stats::sd(x_2)^2.0
      ) / ((length(x_1) + length(x_2)) - 2L)
    )

    ## Degrees of freedom
    df <- length(x_1) + length(x_2) - 2L

    ## t-statistics
    t_lower <- (mean(x_1) - mean(x_2) - (-0.2)) / (sd_pooled * sqrt(1.0 / length(x_1) + 1.0 / length(x_2)))
    t_upper <- (mean(x_1) - mean(x_2) - (0.2)) / (sd_pooled * sqrt(1.0 / length(x_1) + 1.0 / length(x_2)))

    ## p-values
    p_lower <- stats::pt(t_lower, df, lower.tail = FALSE)
    p_upper <- stats::pt(t_upper, df, lower.tail = TRUE)
    p_value <- max(p_lower, p_upper)

    testthat::expect_lte(abs(results_1$`x_2 - x_1`$p_value - p_value), 0.01)
    testthat::expect_lte(p_value, 0.11)
    testthat::expect_gte(p_value, 0.09)

    # Small tolerance.
    results_1 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = 0.2,
      alternative = "not_inferior"
    )


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
