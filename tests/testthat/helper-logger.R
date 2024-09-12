#' Wrap code that invokes the logger
#' 
#' When we load metayer, we set up loggers at index = 1 and index = 2; so, 
#' index = 3 should be available in test.
#' 
#' @param expr the code to wrap
#' @param namespace the namespace to capture
with_wrapped_logger <- function(expr, namespace = "global") {
  withr::with_tempfile(
    "tmp",
    {
      withr::defer(
        delete_logger_index(namespace = namespace, index = 3)
      )
      logger::log_appender(
        appender_file(
          tmp
        ),
        namespace = namespace, 
        index = 3
      )
      force(expr)
      xfun::read_utf8(tmp)
    }
  )  
}
