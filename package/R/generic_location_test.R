#' @include generic_test.R
NULL


# empiricalGenericLocTest object -----------------------------------------------
setClass(
  "empiricalGenericLocTest",
  contains = "empiricalTest",
  slots = list(
    alternative = "character",
    percentiles = "numeric",
    summary_function = "ANY",
    summary_function_args = "ANY"
  ),
  prototype = list(
    alternative = "two.sided",
    percentiles = NA_real_,
    summary_function = NULL,
    summary_function_args = NULL
  )
)


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalGenericLocTest"),
  function(
    .Object,
    data,
    alternative,
    summary_function,
    summary_function_args,
    ...
  ) {
    # Pass to empiricalTest for global attributes.
    .Object <- callNextMethod()

    if (rlang::is_missing(data)) ..error_missing_variable("data")
    if (rlang::is_missing(alternative)) ..error_missing_variable("alternative")
    if (rlang::is_missing(summary_function)) ..error_missing_variable("summary_function")
    if (rlang::is_missing(summary_function_args)) ..error_missing_variable("summary_function_args")
    browser()
    # Format data.
    .Object@input_data <- format_data(data = data)

    # Set the alternative hypothesis.
    .check_parameter_value_is_valid(
      x = alternative,
      var_name = "alternative",
      values = c("two.sided", "two_sided", "greater", "less")
    )
    if (alternative == "two.sided") alternative <- "two-sided"
    .Object@alternative <- alternative

    # Set percentiles.
    .Object <- .set_percentiles(object = .Object)

    # Check summary_function.
    if (!is.null(summary_function)) {
      # 1. That the function is a function.
      .check_is_function(
        x = summary_function,
        var_name = "summary_function"
      )

      # 2. That the function returns a single numeric value.
      y <- do.call(
        summary_function,
        args = c(
          list(data[[1L]]),
          summary_function_args
        )
      )
      .check_is_numeric(
        x = y,
        var_name = "summary_function output"
      )
      .check_argument_length(
        x = y,
        var_name = "summary_function output",
        min = 1L,
        max = 1L
      )
      .Object@summary_function <- summary_function
      .Object@summary_function_args <- summary_function_args
    }

    return(.Object)
  }
)


# .set_percentiles -------------------------------------------------------------

## generic ---------------------------------------------------------------------
setGeneric(".set_percentiles", function(object, ...) standardGeneric(".set_percentiles"))

## empiricalGenericLocTest -----------------------------------------------------
setMethod(
  ".set_percentiles",
  signature(object = "empiricalGenericLocTest"),
  function(
    object,
    ...
  ) {
    if (object@alternative == "two_sided") {
      # group 1 has either smaller or larger summary statistic than group 2
      # by default (0.025, 0.975)
      pctl <- c(0.5 - object@significance_level / 2.0, 0.5 + object@significance_level / 2.0)

    } else if (object@alternative == "less") {
      # group 1 has smaller summary statistic than group 2
      # by default (0.95)
      pctl <- object@significance_level

    } else if (object@alternative == "greater") {
      # group 1 has larger summary statistic than group 2
      # by default (0.05)
      pctl <- 1.0 - object@significance_level

    } else {
      ..error_reached_unreachable_code(paste0("alternative attribute was not correctly set: ", object@alternative))
    }

    object@percentiles <- pctl

    return(object)
  }
)



# ..compute_summary_statistic --------------------------------------------------
setMethod(
  "..compute_summary_statistic",
  signature(object = "empiricalGenericLocTest"),
  function(
    object,
    ...
  ) {
    # Produce samples.
    samples <- .sample_data(object, ...)

    # Apply summary function to all samples.
    values <- mapply(
      FUN = object@summary_function,
      samples,
      MoreArgs = object@summary_function_args,
      SIMPLIFY = FALSE,
      USE.NAMES = TRUE
    )

    return(values)
  }
)


# ..bootstrap_test -------------------------------------------------------------
setMethod(
  "..bootstrap_test",
  signature(object = "empiricalGenericLocTest"),
  function(
    object,
    ...
  ) {
    # Produce samples.
    samples <- .sample_data(object, ...)

    # For bootstraps on summary statistics for location tests, test only the
    # hypothesis: we use this to compute the statistical power, whereas the
    # summary statistics data is used to determine p-values.
    return(.test(object = object, data = samples, hypothesis_test_only = TRUE))
  }
)
