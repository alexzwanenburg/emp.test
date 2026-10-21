# Environment holding cached variables and settings.
emp_test_global_env <- new.env(parent = emptyenv())


.clear_emp_test_global_env <- function() {
  rm(list = ls(emp_test_global_env), envir = emp_test_global_env)
}
