#' @include wrapped-factory.R
NULL

# log_level meta-magic --------------------------------------------------------

get_namespace_name <- function(envir = parent.frame()) {
  s <- topenv(envir) %>% env_name()
  sub("^namespace:(.*)", "\\1", s)
}

#' @inherit logger::log_level
#' @export
log_level <- function(
    level,
    ...,
    namespace = NULL,
    .logcall = sys.call(), 
    .topcall = sys.call(-1),
    .topenv = parent.frame()) {

  namespace <- namespace %||% get_namespace_name(parent.frame())

  logger::log_level(
    level = level,
    ...,    
    namespace = namespace,
    .logcall = .logcall,
    .topcall = .topcall, 
    .topenv = .topenv
  )
}

# wrapped log methods ---------------------------------------------------------

#' @inherit logger::log_debug
#' @export
log_debug <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::DEBUG, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @inherit logger::log_error
#' @export
log_error <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::ERROR, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @inherit logger::log_fatal
#' @export
log_fatal <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::FATAL, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @inherit logger::log_info
#' @export
log_info <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::INFO, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @inherit logger::log_success
#' @export
log_success <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::SUCCESS, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @inherit logger::log_trace
#' @export
log_trace <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::TRACE, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @inherit logger::log_warn
#' @export
log_warn <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::WARN, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}


# mocked log_level calls ------------------------------------------------------

#' A mocked log_level factory
#' 
#' This produces a double wrapping of log_level.  This means that it has the same function
#' signature as logger::log_level, and the call_match function obtains useable default
#' values.
#' 
#' @param logfile location for redirecting log data
mocked_log_level_factory <- function(logfile = stderr(), envir = parent.frame()) {
  if (is.character(logfile) && fs::file_exists(logfile)) {
    fs::file_delete(logfile)
  }

  wrapped_factory(
    "log_level",
    function(cmd, args) {
      msg <- glue(list(...)[[1]], .envir = envir, .null = getOption("mty.cli_null"))
      level <- attr(level, "level")
      msg <- glue("{namespace} {level} {msg}")
      cat(msg, file = logfile, append = TRUE)
    },
    logfile = logfile,
    envir = envir
  )
}

# other stuff -----------------------------------------------------------------

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
with_logger <- function(code, .local_envir = parent.frame(), level = NULL) {

  withCallingHandlers(
    message = function(cnd) {
      msg <- conditionMessage(cnd)
      level <- level %||% logger::INFO
      ns <- get_namespace_name(parent.frame())
      log_level(level, msg, namespace = ns)
    },
    warning = function(cnd) {
      msg <- conditionMessage(cnd)
      level <- level %||% logger::WARN
      ns <- get_namespace_name(parent.frame())
      log_level(level, msg, namespace = ns)
    },
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      level <- level %||% logger::ERROR
      ns <- get_namespace_name(parent.frame())
      log_level(level, msg, namespace = ns)
    },
    eval(code, force(.local_envir))
  )
}
