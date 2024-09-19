#' Reset the logger logs
#' 
#' Warning / Experimental:  this accesses private data in the logger package in
#' an undocumented way
#' 
#' @keywords internal
logger_reset <- function() {
  ns <- grep("^global$", log_namespaces(), value = TRUE, invert = TRUE)

  # workaround for ::: warnings in R CMD check
  logger_namespaces <- get("namespaces", envir = asNamespace("logger"))
  for (key in ns) {
    env_unbind(logger_namespaces, key)
  }
}


#' An appender that will output to jupyter
#' 
#' @keywords internal
appender_jupyter <- function(lines) {
  cat(lines, file = stderr(), sep = "\n")
  inform(lines)
}