#' @include generic_test.R
NULL


# empiricalGenericLocTest object -----------------------------------------------
setClass(
  "empiricalGenericLocTest",
  contains = "empiricalTest",
  slots = list(
    alternative = "character",
    summary_function = "ANY",
    summary_function_args = "ANY"
  ),
  prototype = list(
    alternative = "two.sided",
    summary_function = NULL,
    summary_function_args = NULL
  )
)


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

      return(.Object)
    }
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


