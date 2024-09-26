pubcontext_eval <- function(
    provided_expr,
    context_name,
    raise,
    .envir = parent.frame()) {

  if (raise == TRUE && is_null(provided_expr)) {
    cli_abort("error: pubcontext is '{context_name}' and '{context_name}_expr' is NULL")
  } else if (!is_null(provided_expr)) {
    eval(provided_expr, .envir)
  }
}

.pubcontext <- function(
  jupyter_expr = NULL,
  knitr_expr = NULL,
  rstudio_expr = NULL,
  interactive_expr = NULL,
  non_interactive_expr = NULL,
  raise = FALSE,
  .envir = parent.frame()
) {
  
  is_jupyter <- isTRUE(getOption("jupyter.in_kernel"))
  is_knitr <- isTRUE(getOption("knitr.in.progress"))
  is_rstudio <- Sys.getenv("RSTUDIO") == "1"
  is_interactive <- interactive()

  if (is_jupyter) {
    log_debug(".pubcontext: is_jupyter")
    pubcontext_eval(jupyter_expr, "jupyter", raise, .envir = .envir)

  } else if (is_knitr) {
    log_debug(".pubcontext: is_knitr")
    pubcontext_eval(knitr_expr, "knitr", raise, .envir = .envir)

  } else if (is_rstudio) {
    log_debug(".pubcontext: is_rstudio")
    pubcontext_eval(rstudio_expr, "rstudio", raise, .envir = .envir)

  } else if (is_interactive) {
    log_debug(".pubcontext: is_interactive")
    pubcontext_eval(interactive_expr, "interactive", raise, .envir = .envir)

  } else {
    log_debug(".pubcontext: other")
    pubcontext_eval(non_interactive_expr, "non_interactive", raise, .envir = .envir)
  }
}

#' dispatch contextualized code
#' 
#' Pubcontext dispatches contextualized code.
#' 
#' @param jupyter_code jupyter client code
#' @param knitr_code knitr client code
#' @param rstudio_code rstudio client code
#' @param interactive_code interactive script client code
#' @param non_interactive_code non-interactive script client code
#' @param raise boolean, if TRUE, raise errors
#' @param .envir environment in which to evaluate code block
#' @param .local_envir environment in which to process logs
#' @export
pubcontext <- function(
  jupyter_code = NULL,
  knitr_code = NULL,
  rstudio_code = NULL,
  interactive_code = NULL,
  non_interactive_code = NULL,
  raise = FALSE,
  .envir = parent.frame(),
  .local_envir = parent.frame()
) {
  with_logger(
    {
      .pubcontext(
        substitute(jupyter_code),
        substitute(knitr_code),
        substitute(rstudio_code),
        substitute(interactive_code),
        substitute(non_interactive_code),
        raise = raise,
        .envir = .envir
      )
    },
    .local_envir = .local_envir
  )
}
