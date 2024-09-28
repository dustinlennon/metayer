#' @include wrapped-factory.R
NULL

# log_level meta-magic --------------------------------------------------------

get_namespace_name <- function(envir = parent.frame()) {
  s <- topenv(envir) %>% env_name()
  sub("^namespace:(.*)", "\\1", s)
}

# mocked log_level calls ------------------------------------------------------

#' a mocked log_level factory
#'  
#' @param logfile location for redirecting log data
#' @export
mocked_log_level_factory <- function(logfile = stderr()) {
  if (is.character(logfile) && fs::file_exists(logfile)) {
    fs::file_delete(logfile)
  }

  logger_level <- config_get("logger", "threshold")
  logger_layout <- logger::layout_glue_generator(
    format = config_get("logger", "format")
  )

  function(
      level,
      ...,
      namespace = NA_character_,
      .logcall = sys.call(), 
      .topcall = sys.call(-1),
      .topenv = parent.frame()) {

    if (level <= logger_level) {
      namespace <- if (is.na(namespace)) get_namespace_name(.topenv) else namespace

      msg <- logger::formatter_glue(..., logcall = .logcall, .topcall = .topcall, .topenv = .topenv)

      log_data <- logger_layout(
        level,
        msg,
        namespace = namespace,
        .logcall = .logcall,
        .topcall = .topcall,
        .topenv = .topenv
      )

      xfun::write_utf8(log_data, logfile)
    }
  }
}

# other stuff -----------------------------------------------------------------

#' reset non-global namespaces
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

escape_braces <- function(m) gsub("([{}])", "\\1\\1", m)

#' wrap client code with logging functionality
#' 
#' @param code client code
#' @param .local_envir environment to evaluate client code
#' @param level a logger level; may override condition message defaults
#' @export
with_logger <- function(code, .local_envir = parent.frame(), level = NULL) {

  ns <- get_namespace_name(.local_envir)

  withCallingHandlers(
    message = function(cnd) {
      msg <- conditionMessage(cnd)
      level <- level %||% logger::INFO
      log_level(level, escape_braces(msg), namespace = ns)
    },
    warning = function(cnd) {
      msg <- conditionMessage(cnd)
      level <- level %||% logger::WARN
      log_level(level, escape_braces(msg), namespace = ns)
    },
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      level <- level %||% logger::ERROR
      log_level(level, escape_braces(msg), namespace = ns)
    },
    eval(code, .local_envir)
  ) %>%
    invisible()
}
