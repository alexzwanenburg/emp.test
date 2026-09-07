# mean x_1: 0.122
# mean x_2: 0.004
x_1 <- emp.test:::.rnorm(50L, seed = 18L)
x_2 <- x_1 + 0.1

results <- emp.test::difference_test(
  x_1 = x_1,
  x_2 = x_2
)

results_2 <- stats::t.test(
  x = x_1,
  y = x_2
)
