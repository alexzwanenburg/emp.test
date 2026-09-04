paste_s <- function(...) {
  # Function to collapse a series of strings into a summation in the form
  # "element_1, element_2, ..., and element_n". Originally from the familiar
  # package.
  dots <- c(...)

  if (length(dots) > 2L) {
    # For more than 2 elements, split into an initial and final section.
    initial_string <- paste0(
      head(dots, n = length(dots) - 2L),
      collapse = ", "
    )

    final_string <- paste0(
      tail(dots, n = 2L),
      collapse = " and "
    )

    return(paste0(c(initial_string, final_string), collapse = ", "))

  } else if (length(dots) == 2L) {
    # For exactly 2 elements, combine with "and".
    return(paste0(dots, collapse = " and "))

  } else {
    # For only one element, return as is.
    return(paste0(dots))
  }
}


aggregate_results <- function(x) {
  # Flatten list (is chunked by sets of bootstraps).
  x <- unlist(x, recursive = FALSE)

  # Identify groups.
  groups <- names(x[[1L]])
  group_list <- list()

  for (group in groups) {
    group_results <- lapply(x, function(x, group) (x[[group]]), group = group)
    element_list <- list()

    for (element in names(group_results[[1L]])) {
      element_data <- sapply(group_results, function(x, element) (x[[element]]), element = element)
      if (is.matrix(element_data)) element_data <- t(element_data)
      element_list[[element]] <- element_data
    }
    group_list[[group]] <- element_list
  }

  return(group_list)
}
