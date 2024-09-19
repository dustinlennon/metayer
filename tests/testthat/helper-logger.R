#' Wrap code that invokes the logger
#' 
#' When we load metayer, we set up loggers at index = 1 and index = 2; so, 
#' index = 3 should be available in test.
#' 
#' @param code the code to wrap
#' @param namespace the namespace to capture
#' @param .raise TRUE to raise errors
#' @param .envir the environment in which to evaluate code
#' @return character, possibly NULL
with_wrapped_logger <- function(
    code,
    namespace = "global",
    logfile = NULL,
    .raise = FALSE,
    .envir = parent.frame()) {

  if (is_null(logfile)) {
    logfile <- tempfile()
    withr::defer(
      fs::file_delete(logfile)
    )
  }

  withr::defer(
    delete_logger_index(namespace = namespace, index = 3)
  )

  logger::log_appender(
    appender_file(
      logfile
    ),
    namespace = namespace, 
    index = 3
  )

  result <- NULL
  tryCatch(
    {
      eval(code, envir = .envir)
    },
    error = function(cnd) {
      if (.raise) cnd_signal(cnd)
    },
    warning = function(cnd) {
      if (.raise) cnd_signal(cnd)
    },
    finally = {
      if (fs::file_exists(logfile)) {
        result <- xfun::read_utf8(logfile)
      }
    }
  )

  result
}
