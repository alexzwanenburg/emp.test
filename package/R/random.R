.start_random_number_stream <- function(seed) {
  # From familiar. Starts an rstream.
  if (is.null(seed)) return(NULL)

  r <- methods::new("rstream.runif", seed = seed)

  return(r)
}
