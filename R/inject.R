#' Programmatically construct an external call
#'
#' @param pkg a package name, e.g., "cli"
#' @param name a function in the package, e.g., "cli_text"
#' @param func the function, if available
#' @returns a call that contains the requested command with args
get_cmd_call <- function(pkg, name, func = NULL) {

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

#' Bind an injected function
#' 
#' @param ref the reference function to bind
#' @param cmd_call a cmd call
bind_call <- function(ref, cmd_call, ...) {
  replicated_names <- c(names(list(...)), "cmd_call")
  fml <- formals(ref) %>%
    purrr::discard_at(replicated_names)

  local_env <- new_environment(data = list(...))
  env_bind(
    local_env,
    cmd_call = cmd_call
  )

  mapped_body <- lapply(
    body(ref),
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
#' @param ref the reference function
#' @param pkg the package name
#' @param name the object (function) name
#' @param level the logger level
#' @returns a wrapped function
wrap_factory <- function(
    ref,
    pkg,
    name,
    level) {

  func <- getExportedValue(pkg, name)

  cmd_call <- get_cmd_call(pkg, name, func = func)

  log_trace(
    deparse(cmd_call, width.cutoff = 200L)
  )

  wrapped <- bind_call(
    ref,
    cmd_call,
    level = level,
    .func = func
  )

  formals(wrapped) <- formals(func)

  wrapped
}
