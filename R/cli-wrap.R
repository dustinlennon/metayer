#' Cli wrapper logic
#' 
#' wrap_factory uses the body of this function as a building block.  It
#' does this using 'substitute', replacing the cmd and level parameters 
#' with their known expressions.
#' 
#' Note, the awkward substitute construction:
#' 
#'   local_env <- new_environment(list(cmd = cmd))
#'   injection <- lapply(
#'     body(cli_wrap_body),
#'     function(s) do.call(substitute, list(s, local_env))
#'   ) %>%
#'     as.call()
#' 
#' @param cmd_call a call, the cli operation that we're wrapping
#' @param level the logger level to associated with the cmd
cli_wrap_body <- function(
    cmd_call,
    level,
    .caller_env = caller_env()) {

  mc <- match.call()

  .current_env <- current_env()
  .caller_env <- .current_env$.caller_env %||% caller_env()
  
  wenv <- new_environment(parent = .current_env) %>%
    env_rename("logger_injection")
  env_bind(
    wenv,
    .log_level = level,
    .log_namespace = inj_get_namespace(.caller_env)
  )

  outer_cnd <- catch_cnd(
    {
      # In order to call cli_wrap_body directly and do the metaprogramming
      # substitution, we need this branching logic.
      if ("cmd_call" %in% names(mc)) {
        eval(.current_env$cmd_call, envir = .current_env)
      } else {
        cmd_call
      }
    }
  )

  if (inherits(outer_cnd, "cli_message")) {
    cnd <- outer_cnd
  } else if (inherits(outer_cnd, "condition")) {
    cnd <- catch_cnd(
      cli::cli_verbatim(
        format(outer_cnd, backtrace = FALSE)
      )
    )
  } else {
    msg <- glue::glue("unexpected result: {outer_cnd}", .null = getOption("metayer.cli_null"))
    cnd <- error_cnd(message = msg)
  }

  handler <- getOption("cli.default_handler", metayer_cli_handler)
  withr::with_environment(
    wenv,
    handler(cnd)
  )

  cnd_signal(outer_cnd)
  invisible(NULL)
}


#' Wrap an exported function, safely
#' 
#' @inheritParams wrap_factory
#' @returns a wrapped function
cli_wrap_safe <- function(name, level) {
  pkg <- "cli"
  try_fetch(
    wrap_factory(cli_wrap_body, pkg, name, level),
    error = function(cnd) {      
      getExportedValue(pkg, name)
    }
  )
}
