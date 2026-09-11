# empiricalTest object ---------------------------------------------------------

#' Empirical test object
#'
#' Add description here.
#'
#' @slot name description
#' @slot input_data description
#' @slot summary_data description,
#' @slot significance_level description
#' @slot n_draw_input_samples description
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
    summary_data = "ANY",
    # Significance level of the test.
    significance_level = "numeric",
    # Number of samples that need to be drawn from input data.
    n_draw_input_samples = "integer",
    # Number of samples that need to be drawn from summary data.
    n_draw_summary_samples = "integer",
    # Number of bootstraps,
    n_bootstraps = "integer",
    # Statistical test information, similar to standard output such as from t.test.
    htest = "ANY"
  ),
  prototype = list(
    name = NA_character_,
    input_data = NULL,
    summary_data = NULL,
    significance_level = 0.95,
    n_draw_input_samples = NA_integer_,
    n_draw_summary_samples = NA_integer_,
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
    if (rlang::is_missing(significance_level)) ..error_missing_variable("significance_level")

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



# get_n_draw_input_samples method ----------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric("get_n_draw_input_samples", function(object, ...) standardGeneric("get_n_draw_input_samples"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  "get_n_draw_input_samples",
  signature(
    object = "empiricalTest"
  ),
  function(object) {
    if (is.null(object@input_data)) return(0L)

    # The default is for all samples to resampled.
    return(sum(lengths(object@input_data)))
  }
)

# get_n_draw_summary_samples method --------------------------------------------
setGeneric("get_n_draw_summary_samples", function(object, ...) standardGeneric("get_n_draw_summary_samples"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  "get_n_draw_summary_samples",
  signature(object = "empiricalTest"),
  function(object) {
    if (is.null(object@summary_data)) return(0L)

    # The default is for all summary data to be resampled.
    return(sum(lengths(object@summary_data)))
  }
)



# .sample_data -----------------------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric(".sample_data", function(object, ...) standardGeneric(".sample_data"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  ".sample_data",
  signature(object = "empiricalTest"),
  function(object, random_values, ...) {
    data <- object@summary_data
    if (is.null(data)) data <- object@input_data
    if (is.null(random_values)) return(data)

    # Special case for point estimates.
    if (is.null(random_values)) return(object@input_data)

    # The default is for all samples to be resampled.
    sampled_data <- list()
    index_start <- 1L
    for (variable in names(data)) {
      n_samples <- length(data[[variable]])

      index_end <- index_start + n_samples  - 1L
      sample_indices <- as.integer(ceiling(random_values[index_start:index_end] * n_samples))

      # These edge cases are highlu unlikely but not impossible.
      sample_indices[sample_indices < 1L] <- 1L
      sample_indices[sample_indices > n_samples] <- n_samples

      # Select samples.
      sampled_data[[variable]] <- data[[variable]][sample_indices]

      # Increment random value index counter.
      index_start <- index_end + 1L
    }

    return(sampled_data)
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

    # Distribute bootstraps to runs and compute summary statistics (if
    # required). Note that .compute_summary_statistic is a thin wrapper around
    # ..compute_summary_statistic. .compute_summary_statistic handles the
    # generation of random values based on the bootstraps created by
    # .generate_chunks. ..compute_summary_statistic then computes statistics
    # within each bootstraps.
    if (is.null(object@summary_function)) {
      # Propagate input data to summary data if there is no summary function. This
      # is allowed for tests that do not need a summary function, e.g. mean.
      object@summary_data <- object@input_data

    } else {
      # Determine distribution of bootstraps to available threads.
      bootstraps <- .generate_chunks(
        object = object,
        cl = cl
      )

      if (length(cl) <= 1L) {
        results <- mapply(
          FUN = .compute_summary_statistic,
          bootstraps = bootstraps,
          MoreArgs = list(object = object),
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      } else {
        results <- parallel::clusterMap(
          cl = cl,
          fun = .compute_summary_statistic,
          bootstraps = bootstraps,
          MoreArgs = list(object = object),
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      }
      # Combine summary statistic values into lists.
      results <- unlist(results, recursive = FALSE)
      results <- split(unlist(results), names(object@input_data))
      results <- lapply(results, unname)

      object@summary_data <- results
    }

    # Determine the number of samples that need to be drawn.
    object@n_draw_summary_samples <- get_n_draw_summary_samples(object)

    # Conduct tests on bootstraps of the summary data.
    bootstraps <- .generate_chunks(
      object = object,
      cl = cl
    )

    if (length(cl) <= 1L) {
      results <- mapply(
        FUN = .bootstrap_test,
        bootstraps = bootstraps,
        MoreArgs = list(object = object),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
    } else {
      results <- parallel::clusterMap(
        cl = cl,
        fun = .bootstrap_test,
        bootstraps = bootstraps,
        MoreArgs = list(object = object),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
    }
    results <- aggregate_results(x = results)

    # Get test results and generate htest object.
    htest <- .compute_test_results(object = object, x = results)
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
    n_threads <- max(c(length(cl), 1L))

    # Determine where the bootstraps should operate on.
    if (is.null(object@summary_data)) {
      n_samples <- object@n_draw_input_samples
    } else {
      n_samples <- object@n_draw_summary_samples
    }

    # The maximum number of values that need to be drawn. The main overhead of
    # rstream is starting number generation, not the number of generated
    # numbers. Thus it is more efficient to prestream as many values as possible
    # within each chunk. We only want to prestream at most 10 million values
    # in total to avoid memory issues.
    max_drawn_samples <- n_bootstraps * n_samples
    n_chunks <- as.integer(ceiling(max_drawn_samples / 10E6) * n_threads)

    # Ensure that at each chunk has at least one bootstrap.
    if (n_chunks > n_bootstraps) n_chunks <- n_bootstraps

    # Create chunks.
    if (n_chunks > 1L) {
      chunks <- structure(
        split(
          seq_len(n_bootstraps),
          cut(seq_len(n_bootstraps), n_chunks)
        ),
        names = NULL
      )
    } else {
      chunks <- list(seq_len(n_bootstraps))
    }

    return(chunks)
  }
)



# .compute_summary_statistic ---------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric(".compute_summary_statistic", function(object, ...) standardGeneric(".compute_summary_statistic"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  ".compute_summary_statistic",
  signature(object = "empiricalTest"),
  function(
    object,
    bootstraps
  ) {
    # Draw random values, and then split values for further dispatching.
    r <- .start_random_number_stream(seed=bootstraps[1L])
    n_draw <- length(bootstraps) * object@n_draw_input_samples
    random_values <- rstream::rstream.sample(r, n_draw)

    # Create chunks of random values. Note that this method is faster than
    # structure(split(..., cut(...))).
    random_value_chunks <- list()
    index_start <- 1L
    for (ii in seq_along(bootstraps)) {
      random_value_chunks[[ii]] <- random_values[index_start:(index_start + object@n_draw_input_samples - 1L)]
      index_start <- index_start + object@n_draw_input_samples
    }

    # Dispatch to per-bootstrap routines.
    results <- mapply(
      FUN = ..compute_summary_statistic,
      random_values = random_value_chunks,
      MoreArgs = list("object" = object),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )

    return(results)
  }
)




# ..compute_summary_statistic --------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric("..compute_summary_statistic", function(object, ...) standardGeneric("..compute_summary_statistic"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  "..compute_summary_statistic",
  signature(object = "empiricalTest"),
  function(object, ...) {
    # This needs to be implemented on the level of individual tests.
    ..error_reached_unreachable_code("..compute_summary_statistic should be implemented in subclasses of empiricalTest.")
  }
)


# .bootstrap_test --------------------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric(".bootstrap_test", function(object, ...) standardGeneric(".bootstrap_test"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  ".bootstrap_test",
  signature(object = "empiricalTest"),
  function(
    object,
    bootstraps
  ) {
    # Draw random values, and then split values for further dispatching.
    r <- .start_random_number_stream(seed = bootstraps[1L] + 1L)
    n_draw <- length(bootstraps) * object@n_draw_summary_samples
    random_values <- rstream::rstream.sample(r, n_draw)

    # Create chunks of random values. Note that this method is faster than
    # structure(split(..., cut(...))).
    random_value_chunks <- list()
    index_start <- 1L
    for (ii in seq_along(bootstraps)) {
      random_value_chunks[[ii]] <- random_values[index_start:(index_start + object@n_draw_summary_samples - 1L)]
      index_start <- index_start + object@n_draw_summary_samples
    }

    # Dispatch to per-bootstrap routines.
    results <- mapply(
      FUN = ..bootstrap_test,
      random_values = random_value_chunks,
      MoreArgs = list("object" = object),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )

    return(results)
  }
)


# ..bootstrap_test --------------------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric("..bootstrap_test", function(object, ...) standardGeneric("..bootstrap_test"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  "..bootstrap_test",
  signature(object = "empiricalTest"),
  function(
    object,
    ...
  ) {
    # Produce samples.
    samples <- .sample_data(object, ...)

    return(.test(object = object, data = samples, hypothesis_test_only = FALSE))
  }
)


# .test ------------------------------------------------------------------------
setGeneric(".test", function(object, ...) standardGeneric(".test"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  ".test",
  signature(object = "empiricalTest"),
  function(object, data, ...) {
    # This needs to be implemented on the level of individual tests.
    ..error_reached_unreachable_code(".test should be implemented in subclasses of empiricalTest.")
  }
)

# ..test -----------------------------------------------------------------------
setGeneric("..test", function(object, ...) standardGeneric("..test"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  "..test",
  signature(object = "empiricalTest"),
  function(object, ...) {
    # This needs to be implemented on the level of individual tests.
    ..error_reached_unreachable_code("..test should be implemented in subclasses of empiricalTest.")
  }
)





# .compute_test_results --------------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric(".compute_test_results", function(object, ...) standardGeneric(".compute_test_results"))

## empiricalTest ---------------------------------------------------------------
setMethod(
  ".compute_test_results",
  signature(object = "empiricalTest"),
  function(object, ...) {
    # This needs to be implemented on the level of individual tests.
    ..error_reached_unreachable_code(".compute_test_results should be implemented in subclasses of empiricalTest.")
  }
)
