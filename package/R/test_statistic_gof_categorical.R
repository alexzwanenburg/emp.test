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
