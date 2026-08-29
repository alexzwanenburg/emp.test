# empiricalTest object ---------------------------------------------------------

#' Empirical test object
#'
#' Add description here.
#'
#' @slot name description
#' @slot input_data
#' @slot output_data description
#' @slot significance_level description
#' @slot n_draw_samples description
#' @slot n_bootstraps description
#' @slot htest description
#'
#' @export

setClass(
  "empiricalTest",
  slots = list(
    # Name of the test.
    name = "character",
    # Copy of input data.
    input_data = "ANY",
    # Intermediate output data.
    output_data = "ANY",
    # Significance level of the test.
    significance_level = "numeric",
    # Number of samples that need to be drawn.
    n_draw_samples = "integer",
    # Number of bootstraps,
    n_bootstraps = "integer",
    # Statistical test information, similar to standard output such as from t.test.
    htest = "ANY"
  ),
  prototype = list(
    name = NA_character_,
    input_data = NULL,
    output_data = NULL,
    significance_level = 0.95,
    n_draw_samples = NA_integer_,
    n_bootstraps = 0L,
    htest = NULL
  )
)

# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalTest"),
  function(
    .Object,
    significance_level,
    ...
  ) {

    # Check significance. Significance level cannot be equal to 1.0, because
    # that would not be meaningful.
    .check_number_in_valid_range(
      x = significance_level,
      var_name = "significance_level",
      range = c(0.0, 1.0),
      closed = c(TRUE, FALSE)
    )
    .Object@significance_level <- significance_level

    return(.Object)
  }
)


# get_n_draw_samples method ----------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric("get_n_draw_samples", function(object, ...) standardGeneric("get_n_draw_samples"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  "get_n_draw_samples",
  signature(
    object = "empiricalTest"
  ),
  function(object) {
    if (is.null(object@input_data)) return(0L)

    # The default is for all samples to resampled.
    return(sum(lengths(object@input_data)))
  }
)



# run_test method --------------------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric("run_test", function(object, ...) standardGeneric("run_test"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  "run_test",
  signature(
    object = "empiricalTest"
  ),
  function(
    object,
    cl = NULL
  ) {
    # Determine distribution of bootstraps to available threads.
    bootstraps <- .generate_chunks(
      object = object,
      cl = cl
    )

    # Distribute bootstraps to runs and compute test statistics.
    if (length(cl) <= 1L) {
      results <- mapply(
        FUN = .compute_statistic,
        bootstraps = bootstraps,
        MoreArgs = list(object = object),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
    } else {
      results <- parallel::clusterMap(
        cl = cl,
        fun = .compute_statistic,
        bootstraps = bootstraps,
        MoreArgs = list(object = object),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
    }
    results <- unlist(results, recursive = FALSE)
    object@output_data <- results

    # Get test results.
    htest <- .compute_test_results(object = object)
    object@htest <- htest

    return(object)
  }
)



# .generate_chunks -------------------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric(".generate_chunks", function(object, ...) standardGeneric(".generate_chunks"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  ".generate_chunks",
  signature(
    object = "empiricalTest"
  ),
  function(
    object,
    cl = NULL,
    n_bootstraps = NULL
  ) {
    # The chunker creates a nested list of bootstrap ids that are then
    # distributed to threads.

    # Set number of bootstraps.
    if (is.null(n_bootstraps)) n_bootstraps <- object@n_bootstraps

    # Determine the number of available threads (at least 1).
    n_threads <- min(c(length(cl), 1L))

    # The maximum number of values that need to be drawn. The main overhead of
    # rstream is starting number generation, not the number of generated
    # numbers. Thus it is more efficient to prestream as many values as possible
    # within each chunk. We only want to prestream at most 10 million values
    # in total to avoid memory issues.
    max_drawn_samples <- n_bootstraps * object@n_draw_samples
    n_chunks <- as.integer(ceiling(max_drawn_samples / 10E6) * n_threads)

    # Ensure that at each chunk has at least one bootstrap.
    if (n_chunks > n_bootstraps) n_chunks <- n_bootstraps

    # Create chunks.
    chunks <- structure(
      split(
        seq_len(n_bootstraps),
        cut(seq_len(n_bootstraps), n_chunks)
      ),
      names = NULL
    )

    return(chunks)
  }
)



# .compute_statistic -----------------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric(".compute_statistic", function(object, ...) standardGeneric(".compute_statistic"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  ".compute_statistic",
  signature(
    object = "empiricalTest"
  ),
  function(
    object,
    bootstraps
  ) {
    # Draw random values, and then split values for further dispatching.
    r <- .start_random_number_stream(seed=bootstraps[1L])
    n_draw <- length(bootstraps) * object@n_draw_samples
    random_values <- rstream::rstream.sample(r, n_draw)

    # Chunks.
    random_values <- structure(
      split(
        random_values,
        cut(seq_along(random_values), length(bootstraps))
      ),
      names = bootstraps
    )

    # Dispatch to per-bootstrap routines.
  }
)
