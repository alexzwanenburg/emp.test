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

    # Distribute bootstraps to runs.

    # Return output for bootstraps.
  }
)
