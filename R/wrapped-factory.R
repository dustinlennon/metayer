#' @include utils-env.R
NULL

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

#' Produce a wrapped function
#' 
#' A wrapper should have the function signature:
#' 
#'   function(cmd, args, ..., .name = NULL)
#' 
#' where ... may, optionally, be replaced with named parameters.
#' 
#' Moreover, at some point in its definition, a wrapper should called the wrapped 
#' function with its args:
#' 
#'    do.call(cmd, args)
#' 
#' @param name the function name, a string
#' @param wrapper the wrapper, a function
#' @param ... (named) parameters in the execution stack; i.e., private to the wrapper
#' @export
wrapped_factory <- function(name, wrapper, ...) {
  symb <- str2lang(name)
  fn <- eval(symb, envir = parent.frame())

  # copy the function signature
  w <- function() NULL
  formals(w) <- fn_fmls(fn)

  # substitution environment:  just cmd and args
  senv <- new_environment(
    list(
      cmd = symb,
      args = fn_fmls_names(fn) %||% list() %>%
        set_names() %>%
        lapply(sym)
    )
  )

  # set the body w/ cmd and args substitutions
  body(w) <- do.call(
    substitute,
    list(
      body(wrapper),
      senv
    )
  )

  # enable access to ... by introducing another frame
  added_frame <- new_environment(
    list(.name = name, ...),
    parent = caller_env()
  )
  obj_addr <- obj_address(added_frame)
  added_frame <- env_rename(added_frame, name = glue("wrapped-{obj_addr}"))

  # merge in key-value items (if any) from the wrapper
  default_params <- fn_fmls(wrapper) %>%
    purrr::discard_at(c("cmd", "args"))

  env_coalesce(added_frame, as.environment(default_params))
  set_env(w, added_frame)
}


#' Remap symbols in a list
#' 
#' @param args the passed args, a list of symb = symb
#' @return a list where the value symbols have been evaluated
#' @export
remap_symb <- function(args, envir = parent.frame()) {
  args %>%
    purrr::map(
      \(sym) get0(sym, envir = envir)
    )
}
