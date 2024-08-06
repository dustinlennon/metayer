#' Reset the logger logs
#' 
#' Warning / Experimental:  this accesses private data in the logger package in
#' an undocumented way
#' 
#' @export
logger_reset <- function() {
  ns <- grep("^global$", log_namespaces(), value = TRUE, invert = TRUE)
  for (key in ns) {
    env_unbind(logger:::namespaces, key)
  }
}
