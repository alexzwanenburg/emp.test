testthat::test_that(
  "Paired equivalence test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)
    x_2 <- x_1 + 0.1 + emp.test:::.rnorm(50L, sd = 0.2, seed = 20L)

    # Two one-sided test procedure.
    ## Compute pooled standard deviation.
    # sd_pooled <- sqrt(stats::sd(x_1)^2.0 + stats::sd(x_2)^2.0) / length(x_1)
    sd_pooled <- stats::sd(x_1 - x_2)

    ## Degrees of freedom
    df <- length(x_1) - 1L

    ## t-statistics
    t_lower <- (mean(x_1 - x_2) - (-0.1)) / (sd_pooled / sqrt(length(x_1)))
    t_upper <- (mean(x_1 - x_2) - (0.1)) / (sd_pooled / sqrt(length(x_1)))

    ## p-values
    p_lower <- stats::pt(t_lower, df, lower.tail = FALSE)
    p_upper <- stats::pt(t_upper, df, lower.tail = TRUE)
    p_value <- max(p_lower, p_upper)

    # Comparison with two one-sided test procedure.
    results_1 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = 0.1,
      paired = TRUE
    )

    testthat::expect_lte(abs(results_1$`x_2 - x_1`$p_value - p_value), 0.01)
    testthat::expect_lte(p_value, 0.30)
    testthat::expect_gte(p_value, 0.29)

    # Larger tolerance --> lower p-value
    results_2 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = 0.3,
      paired = TRUE
    )
    testthat::expect_gt(results_1$`x_2 - x_1`$p_value, results_2$`x_2 - x_1`$p_value)

    # With two tolerances.
    results_3 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = c(-0.3, 0.1),
      paired = TRUE
    )
    testthat::expect_equal(results_1$`x_2 - x_1`$p_value, results_3$`x_2 - x_1`$p_value)

    # Tolerance using a function -- in this case the same tolerance as above.
    tol_fun <- function(x1, x2) (return(c(-0.3, 0.1)))
    results_4 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = tol_fun,
      paired = TRUE
    )
    testthat::expect_equal(results_1$`x_2 - x_1`$p_value, results_4$`x_2 - x_1`$p_value)

    # Tolerance using a function with extra arguments.
    tol_fun <- function(x1, x2, flag) (if (flag) return(c(-0.3, 0.1)) else return(c(-0.05, 0.05)))
    results_5 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = TRUE),
      paired = TRUE
    )
    testthat::expect_equal(results_1$`x_2 - x_1`$p_value, results_5$`x_2 - x_1`$p_value)

    results_6 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = FALSE),
      paired = TRUE
    )
    testthat::expect_gt(results_6$`x_2 - x_1`$p_value, results_5$`x_2 - x_1`$p_value)
  }
)


testthat::test_that(
  "Non-inferiority test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)
    x_2 <- x_1

    # Comparison with two one-sided test procedure.
    results_1 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = -0.2,
      alternative = "not_inferior"
    )

    testthat::expect_lte(results_1$`x_2 - x_1`$p_value, 0.11)
    testthat::expect_gte(results_1$`x_2 - x_1`$p_value, 0.09)

    # Larger tolerance --> lower p-value
    results_2 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = -0.3,
      alternative = "not_inferior"
    )
    testthat::expect_gt(results_1$`x_2 - x_1`$p_value, results_2$`x_2 - x_1`$p_value)

    # With two tolerances (technically not ok)
    results_3 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = c(-0.2, 0.3),
      alternative = "not_inferior"
    )
    testthat::expect_equal(results_1$`x_2 - x_1`$p_value, results_3$`x_2 - x_1`$p_value)

    # Tolerance using a function -- in this case the same tolerance as above.
    tol_fun <- function(x1, x2) (return(-0.2))
    results_4 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = tol_fun,
      alternative = "not_inferior"
    )
    testthat::expect_equal(results_1$`x_2 - x_1`$p_value, results_4$`x_2 - x_1`$p_value)

    # Tolerance using a function with extra arguments.
    tol_fun <- function(x1, x2, flag) (if (flag) return(c(-0.2, 0.3)) else return(c(-0.1, 0.1)))
    results_5 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = TRUE),
      alternative = "not_inferior"
    )
    testthat::expect_equal(results_1$`x_2 - x_1`$p_value, results_5$`x_2 - x_1`$p_value)

    results_6 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = FALSE),
      alternative = "not_inferior"
    )
    testthat::expect_gt(results_6$`x_2 - x_1`$p_value, results_5$`x_2 - x_1`$p_value)
  }
)


testthat::test_that(
  "Non-superiority test functions correctly",
  {
    x_1 <- emp.test:::.rnorm(50L, seed = 19L)
    x_2 <- x_1

    # Comparison with two one-sided test procedure.
    results_1 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = 0.2,
      alternative = "not_superior"
    )

    testthat::expect_lte(results_1$`x_2 - x_1`$p_value, 0.11)
    testthat::expect_gte(results_1$`x_2 - x_1`$p_value, 0.09)

    # Larger tolerance --> lower p-value
    results_2 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = 0.3,
      alternative = "not_superior"
    )
    testthat::expect_gt(results_1$`x_2 - x_1`$p_value, results_2$`x_2 - x_1`$p_value)

    # With two tolerances (technically not ok)
    results_3 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = c(-0.2, 0.2),
      alternative = "not_superior"
    )
    testthat::expect_equal(results_1$`x_2 - x_1`$p_value, results_3$`x_2 - x_1`$p_value)

    # Tolerance using a function -- in this case the same tolerance as above.
    tol_fun <- function(x1, x2) (return(0.2))
    results_4 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = tol_fun,
      alternative = "not_superior"
    )
    testthat::expect_equal(results_1$`x_2 - x_1`$p_value, results_4$`x_2 - x_1`$p_value)

    # Tolerance using a function with extra arguments.
    tol_fun <- function(x1, x2, flag) (if (flag) return(c(-0.3, 0.2)) else return(c(-0.1, 0.1)))
    results_5 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = TRUE),
      alternative = "not_superior"
    )
    testthat::expect_equal(results_1$`x_2 - x_1`$p_value, results_5$`x_2 - x_1`$p_value)

    results_6 <- emp.test::equivalence_test(
      x_1 = x_1,
      x_2 = x_2,
      tolerance = tol_fun,
      tolerance_function_args = list("flag" = FALSE),
      alternative = "not_superior"
    )
    testthat::expect_gt(results_6$`x_2 - x_1`$p_value, results_5$`x_2 - x_1`$p_value)
  }
)



testthat::test_that(
  "Equivalence test functions correctly with more than 2 variables",
  {

  }
)
