#' Wrapped namespace
#' 
#' @keywords internal
#' @param .caller_env the caller environment
#' @returns the namespace
wrap_get_namespace <- function(.caller_env) {
  namespace <- environmentName(topenv(.caller_env))
  if (namespace == "R_GlobalEnv") {
    "global.cli" 
  } else {
    stringr::str_glue("{namespace}.cli")
  }
}

#' Wrapper for a cli method
#' 
#' This includes consideration for cli_inform, cli_warn, and cli_abort which 
#' use an rlang codepath.
#' 
#' @param cmd a cli function, e.g., cli::cli_alert
#' @param args the args to be passed to the cmd via do.call
#' @param level the logger level to be associated with the cli method
wrapper_cli <- function(
    cmd,
    args,
    level) {

  metadata <- list(
    level = level,
    namespace = wrap_get_namespace(caller_env())
  )

  cnd <- catch_cnd(
    do.call(cmd, args)
  )  

  cnd_text <- if (inherits(cnd, "cli_message")) {
    capture_cli_message(cnd)
  } else if (inherits(cnd, "condition")) {
    purrr::pluck(cnd, "message") %>%
      cli::ansi_strip()
  } else {
    "unknown cnd type encountered in wrapper_cli"
  }

  cli::cli_verbatim(
    cnd_text
  )
}

#' Map a wrapped cli condition to its source
#' 
#' Calling catch_cnd on a wrapped cli condition will return a verbatim cli message
#' which masks the source condition.  This method extracts that source condition.
#' 
#' @param cnd a wrapped cli message
wcli_cnd <- function(wcnd) {
  purrr::pluck(wcnd, "args", ".envir", "cnd") %||% wcnd
}

