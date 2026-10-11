test_statistic_chi_square <- function(
  obs,
  exp
) {
  return(sum((obs - exp)^2 / exp))
}
