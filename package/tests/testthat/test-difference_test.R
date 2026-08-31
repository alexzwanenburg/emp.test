results <- emp.test::emp_diff_test(
  x_1 = stats::rnorm(50L),
  x_2 = stats::rnorm(50L, mean = 0.1)
)
