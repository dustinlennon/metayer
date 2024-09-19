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


#' An appender for jupyter (with stderr)
#' 
#' In jupyter, stderr will show up on the console, and messages will show up
#' in the notebook.  We emit messages only in the jupyter context to avoid 
#' double entries.
#' 
#' @keywords internal
appender_jupyter <- function(lines) {
  cat(lines, file = stderr(), sep = "\n")
  if (isTRUE(getOption("jupyter.in_kernel"))) {
    inform(lines)
  }
}
