test_statistic_chi_square <- function(
  observed,
  expected
) {
  # Exclude elements where obs == exp. This also avoids obs == exp == 0, which
  # Would otherwise cause issues.
  equal <- observed == expected
  if (all(equal)) return(0.0)

  return(sum((observed[!equal] - expected[!equal])^2.0 / expected[!equal]))
}



test_statistic_g <- function(
    observed,
    expected
) {
  # Exclude elements where obs == exp. This also avoids obs == exp == 0, which
  # Would otherwise cause issues. Also exclude any obs == 0, because these do
  # not contribute.
  equal_or_zero <- (observed == expected) | (observed == 0L)
  if (all(equal_or_zero)) return(0.0)

  return(2.0 * sum(observed[!equal_or_zero] * log(observed[!equal_or_zero] / expected[!equal_or_zero])))
}



test_statistic_cressie_read <- function(
    observed,
    expected,
    lambda = 2.0 / 3.0
) {
  # Default lambda value after Cressie, N. & Read, T. R. C. Multinomial
  # goodness-of-fit tests. J. R. Stat. Soc. Series B Stat. Methodol. 46, 440–464 (1984).
  if (lambda == 0.0) {
    return(test_statistic_g(observed = observed, expected = expected))

  } else if (lambda == -1.0) {
    rlang::abort(
      message = "lambda = -1.0 is not defined for the Cressie-Read test statistic",
      class = c("emp_test_error", "input_argument_error")
    )
  }

  # Exclude elements where obs == exp. This also avoids obs == exp == 0, which
  # Would otherwise cause issues. Also exclude any obs == 0, because these do
  # not contribute.
  equal_or_zero <- (observed == expected) | (observed == 0L)
  if (all(equal_or_zero)) return(0.0)

  return(
    2.0 / (lambda * (lambda + 1.0)) *
      sum(observed[!equal_or_zero] * ((observed[!equal_or_zero] / expected[!equal_or_zero])^lambda - 1.0))
  )
}
