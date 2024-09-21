#' @include wrapper.R
NULL

# log_level meta-magic --------------------------------------------------------

#' @inherit logger::log_level
#' @export
log_level <- wrapped_factory("logger::log_level", wrapper_mockable) 

#' A mocked log_level factory
#' 
#' This produces a double wrapping of log_level.  This means that it has the same function
#' signature as logger::log_level, and the call_match function obtains useable default
#' values.
#' 
#' @param logfile location for redirecting log data
mocked_log_level_factory <- function(logfile = stderr()) {
  if (is.character(logfile) && fs::file_exists(logfile)) {
    fs::file_delete(logfile)
  }

  wrapped_factory(
    "log_level",
    function(cmd, args) {
      msg <- glue(list(...)[[1]], envir = .topenv, .null = getOption("mty.cli_null"))
      level <- attr(level, "level")
      msg <- glue("{namespace} {level} {msg}")
      cat(msg, file = logfile, append = TRUE)
    },
    logfile = logfile
  )
}

# wrapped log methods ---------------------------------------------------------

# > getNamespaceExports("logger") %>% grep("^[A-Z]+$", ., value=TRUE) %>% sort()
# [1] "DEBUG"   "ERROR"   "FATAL"   "INFO"    "OFF"     "SUCCESS" "TRACE"   "WARN"   

#' @inherit logger::log_debug
#' @export
log_debug <- function(...) {
  log_level(logger::DEBUG, ...)
}

#' @inherit logger::log_error
#' @export
log_error <- function(...) {
  log_level(logger::ERROR, ...)
}

#' @inherit logger::log_fatal
#' @export
log_fatal <- function(...) {
  log_level(logger::FATAL, ...)
}

#' @inherit logger::log_info
#' @export
log_info <- function(...) {
  log_level(logger::INFO, ...)
}

#' @inherit logger::log_success
#' @export
log_success <- function(...) {
  log_level(logger::SUCCESS, ...)
}

#' @inherit logger::log_trace
#' @export
log_trace <- function(...) {
  log_level(logger::TRACE, ...)
}

#' @inherit logger::log_warn
#' @export
log_warn <- function(...) {
  log_level(logger::WARN, ...)
}


#' Reset the logger logs
#' 
#' Warning / Experimental:  this accesses private data in the logger package in
#' an undocumented way
#' 
#' @keywords internal
logger_reset <- function() {
  ns <- grep("^global$", logger::log_namespaces(), value = TRUE, invert = TRUE)

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

#' Evaluate client code with logging
#' 
#' @param code client code
#' @param .envir environment to evaluate client code
#' @param level a logger level; may override condition message defaults
#' @export
with_logger <- function(code, .envir = parent.frame(), level = NULL) {
  .expr <- substitute(code)

  withCallingHandlers(
    message = function(cnd) {
      msg <- cli::format_message(cnd)
      level <- level %||% logger::INFO
      ns <- topenv() %>% env_name()
      log_level(level, msg, namespace = ns)
    },
    warning = function(cnd) {
      msg <- cli::format_warning(cnd)
      level <- level %||% logger::WARN
      ns <- topenv() %>% env_name()
      log_level(level, msg, namespace = ns)
    },
    error = function(cnd) {
      msg <- cli::format_error(cnd)
      level <- level %||% logger::ERROR
      ns <- topenv() %>% env_name()
      log_level(level, msg, namespace = ns)
    },
    eval(.expr, envir = .envir)
  )
}
