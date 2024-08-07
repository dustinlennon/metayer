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
#'     body(wrap_cli_body),
#'     function(s) do.call(substitute, list(s, local_env))
#'   ) %>%
#'     as.call()
#' 
#' @param cmd_call a call, the cli operation that we're wrapping
#' @param level the logger level to associated with the cmd
#' @param .caller_env the caller environment in which to evaluate cmd_call
#' @export
wrap_cli_body <- function(
    cmd_call,
    level,
    .caller_env = caller_env()) {

  # Apply some unconventional parameter initialization logic as this may
  # be called directly or after a substitution
  mc <- match.call()
  .current_env <- current_env()
  .caller_env <- .current_env$.caller_env %||% caller_env()
  
  # Execute the provided cli command.  Note that the handler isn't called if 
  # the condition is caught.
  outer_cnd <- catch_cnd(
    {
      # The branching logic here allows wrap_cli_body to be called directly
      # and also via metaprogramming substitution.
      if ("cmd_call" %in% names(mc)) {
        eval(.current_env$cmd_call, envir = .current_env)
      } else {
        cmd_call
      }
    }
  )

  # Our handler is designed for cli_messages; for standard conditions--e.g. rlang::abort()--
  # we pipe them through cli_verbatim.
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

  # Introduce an additional execution frame to make metadata available 
  # to the handler
  wenv <- new_environment(parent = .current_env) %>%
    env_rename("logger_injection")
  env_bind(
    wenv,
    .log_level = level,
    .log_namespace = inj_get_namespace(.caller_env)
  )

  # Handle the condition, potentially sending to the logger
  handler <- getOption("cli.default_handler", metayer_cli_handler)
  withr::with_environment(
    wenv,
    handler(cnd)
  )

  # Reraise the original signal
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
    wrap_factory(wrap_cli_body, pkg, name, level),
    error = function(cnd) {      
      getExportedValue(pkg, name)
    }
  )
}
