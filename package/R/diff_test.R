#' @include generic_test.R
NULL

emp_diff_test <- function(
    ...,
    alternative = "two.sided",
    significance_level = 0.95,
    summary_function = mean,
    summary_args = list()
) {
  # Create test object.
  
  # Perform checks on input (...)
  dots <- list(...)
  
  # Perform the test.
  
  # Return test information.
}



# empiricalDiffTest object -----------------------------------------------------
setClass(
  "empiricalDiffTest",
  contains = "empiricalTest",
  slots = list(
    alternative = "character"
  ),
  prototype = list(
    name = "empirical difference test",
    alternative = "two.sided"
  )
)


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "empiricalDiffTest"),
  function(
    .Object,
    significance_level,
    alternative,
    ...
  ) {
    # Update with parent class first.
    .Object <- callNextMethod()
    
    # Set the alternative hypothesis.
    .Object@alternative <- "alternative"
    
    return(.Object)
  }
)
