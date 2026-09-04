set.seed(19L)

# mean x_1: 0.122
# mean x_2: 0.004
x_1 <- stats::rnorm(50L)
x_2 <- stats::rnorm(50L, mean = 0.1)

results <- emp.test::emp_diff_test(
  x_1 = x_1,
  x_2 = x_2
)

results_2 <- stats::t.test(
  x = x_1,
  y = x_2
)
