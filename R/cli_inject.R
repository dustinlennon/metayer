#' @include utils-generic.R
NULL

#' Programmatically construct an external call
#'
#' @param pkg a package name, e.g., "cli"
#' @param name a function in the package, e.g., "cli_text"
#' @param func the function, if available
#' @returns a call that contains the requested command with args
inj_get_cmd <- function(pkg, name, func = NULL) {

  if (is.null(func)) {
    func <- getExportedValue(pkg, name)
  }

  fml <- formals(func)

  w_args <- names(fml) %>%
    set_names() %>%
    purrr::map(
      \(o) str2lang(o)
    ) %>%
    as.pairlist()

  # special case naming for dots
  nm <- names(w_args) %>%
    purrr::modify_if(
      \(p) p == "...",
      \(v) NA
    )

  names(w_args) <- nm

  qual_name <- str2lang(stringr::str_glue("{pkg}::{name}"))
  as.call(c(qual_name, w_args))
}

#' Get runtime injections
#' 
#' @param .caller_env the caller environment
#' @returns the namespace
inj_get_namespace <- function(.caller_env) {
  namespace <- environmentName(topenv(.caller_env))
  if (namespace == "R_GlobalEnv") {
    "global.cli" 
  } else {
    stringr::str_glue("{namespace}.cli")
  }
}

#' Inject code body
#' 
#' wrap_factory uses the body of this function as a building block.  It
#' does this using 'substitute', replacing the cmd and level parameters 
#' with their known expressions.
#' 
#' Note, the awkward substitute construction:
#' 
#'   local_env <- new_environment(list(cmd = cmd))
#'   injection <- lapply(
#'     body(inj_body_cli),
#'     function(s) do.call(substitute, list(s, local_env))
#'   ) %>%
#'     as.call()
#' 
#' @param cmd a call, the cli operation that we're wrapping
#' @param level the logger level to associated with the cmd
inj_body_cli <- function(
    cmd,
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
      # In order to call inj_body_cli directly and do the metaprogramming
      # substitution, we need this branching logic.
      if ("cmd" %in% names(mc)) {
        eval(.current_env$cmd, envir = .current_env)
      } else {
        cmd
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
    msg <- glue::glue("unexpected result: {outer_cnd}", .null = getOption("metayer.cli.null"))
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

#' Bind an injected function
#' 
bind_cmd <- function(ref_func, cmd, ...) {
  replicated_names <- c(names(list(...)), "cmd")
  fml <- formals(ref_func) %>%
    purrr::discard_at(replicated_names)

  local_env <- new_environment(data = list(...))
  env_bind(
    local_env,
    cmd = cmd
  )

  mapped_body <- lapply(
    body(ref_func),
    function(s) do.call(substitute, list(s, local_env))
  ) %>%
    as.call()

  wrapped <- function() {}
  body(wrapped) <- mapped_body
  environment(wrapped) <- topenv()
  formals(wrapped) <- fml

  wrapped
}

#' Wrap an exported function
#' 
#' @param pkg the package name
#' @param name the object (function) name
#' @param level the logger level
#' @returns a wrapped function
wrap_factory <- function(
    pkg,
    name,
    level) {

  func <- getExportedValue(pkg, name)

  cmd <- inj_get_cmd(pkg, name, func = func)

  log_trace(
    deparse(cmd, width.cutoff = 200L)
  )

  wrapped <- bind_cmd(
    inj_body_cli,
    cmd,
    level = level,
    .func = func
  )

  formals(wrapped) <- formals(func)

  wrapped
}


#' Wrap an exported function, safely
#' 
#' @inheritParams wrap_factory
#' @returns a wrapped function
wrap_factory_safe <- function(pkg, name, level) {
  try_fetch(
    wrap_factory(pkg, name, level),
    error = function(cnd) {
      getExportedValue(pkg, name)
    }
  )
}
