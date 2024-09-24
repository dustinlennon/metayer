#' wrapped log functions
#' 
#' These functions are wrapped versions of those in the [logger](https://daroczig.github.io/logger/) package.

#' @rdname wrapped_logger
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

#' @rdname wrapped_logger
#' @inherit logger::log_debug
#' @export
log_debug <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::DEBUG, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @rdname wrapped_logger
#' @inherit logger::log_error
#' @export
log_error <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::ERROR, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @rdname wrapped_logger
#' @inherit logger::log_fatal
#' @export
log_fatal <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::FATAL, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @rdname wrapped_logger
#' @inherit logger::log_info
#' @export
log_info <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::INFO, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @rdname wrapped_logger
#' @inherit logger::log_success
#' @export
log_success <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::SUCCESS, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @rdname wrapped_logger
#' @inherit logger::log_trace
#' @export
log_trace <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::TRACE, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}

#' @rdname wrapped_logger
#' @inherit logger::log_warn
#' @export
log_warn <- function(..., namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) { #nolint
  log_level(logger::WARN, ..., namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
}
