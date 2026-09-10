.start_random_number_stream <- function(seed) {
  # From familiar. Starts an rstream.
  if (is.null(seed)) {
    r <- methods::new("rstream.runif")

  } else {
    r <- methods::new("rstream.runif", seed = seed)
  }

  return(r)
}



.ragn <- function(
    n,
    location = 0.0,
    scale = 1.0,
    alpha = 0.5,
    beta = 2.0,
    seed = NULL
) {
  # Random values drawn according to an asymmetric generalised normal
  # distribution. Here the asymmetric generalised normal distribution is a
  # symmetric general normal distribution, that is made asymmetric using the
  # procedure described by Gijbels et al. To generate random values we require
  # the quantile function of the symmetric generalised normal distribution,
  # which is was derived by M. Griffin.
  # This function was adapted from the power.transform package.

  # alpha needs to be in (0, 1).
  if (alpha < 0.0 || alpha > 1.0) stop("alpha should be between 0.0 and 1.0.")

  # Beta cannot be zero or negative.
  if (beta <= 0.0) stop("beta must be strictly positive.")

  # Scale cannot be zero or negative.
  if (scale <= 0.0) stop("scale must be strictly positive.")

  # Start rstream.
  rstream <- .start_random_number_stream(seed = seed)

  # Draw n values from an uniform distribution. Avoid extreme tails for
  # numerical reasons.
  p <- rstream::rstream.sample(rstream, n)
  p[p < sqrt(.Machine$double.eps)] <- sqrt(.Machine$double.eps)
  p[p > 1.0 - sqrt(.Machine$double.eps)] <- 1.0 - sqrt(.Machine$double.eps)

  # Avoid extreme alpha. There are singularities at 0 and 1.
  if (alpha < sqrt(.Machine$double.eps)) {
    alpha <- sqrt(.Machine$double.eps)

  } else if (alpha > 1.0 - sqrt(.Machine$double.eps)) {
    alpha <- 1.0 - sqrt(.Machine$double.eps)
  }

  # Identify values of p which are less and greater than alpha.
  p_less <- which(p <= alpha)
  p_greater <- which(p > alpha)

  # Compute p for the quantile function of the symmetric generalised normal
  # distribution.
  p_sgnd <- numeric(n)
  p_sgnd[p_less] <- p[p_less] / (2.0 * alpha)
  p_sgnd[p_greater] <- (1.0 + p[p_greater] - 2.0 * alpha) / (2.0 * (1.0 - alpha))

  # Compute values according to the quantile function of the symmetric
  # generalised normal distribution.
  x_sgnd <- sign(p_sgnd - 0.5) * stats::qgamma(
    p = 2.0 * abs(p_sgnd - 0.5),
    shape = 1.0 / beta,
    scale = 1.0
  )^(1.0 / beta)

  # Populate array of values x.
  x <- numeric(n)
  x[p_less] <- location + scale / (1.0 - alpha) * x_sgnd[p_less]
  x[p_greater] <- location + scale / alpha * x_sgnd[p_greater]

  return(x)
}



.rnorm <- function(
    n,
    mean = 0.0,
    sd = 1.0,
    seed = NULL
) {
  # Normal distribution is a special case of an asymmetric generalised normal
  # distribution.
  return(.ragn(
    n = n,
    location = mean,
    scale = sd / sqrt(2.0),
    alpha = 0.5,
    beta = 2.0,
    seed = seed
  ))
}
