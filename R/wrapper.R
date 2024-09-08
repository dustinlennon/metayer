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

wrapped_factory <- function(name, wrapper, ...) {
  symb <- str2lang(name)
  fn <- eval(symb, envir = parent.frame())

  w <- function() NULL
  formals(w) <- fn_fmls(fn)

  # environment for substitution
  esub <- env(
    cmd = symb,
    args = fn_fmls_names(fn) %>% set_names() %>% lapply(sym)
  )

  body(w) <- do.call(
    substitute,
    list(
      body(wrapper),
      esub
    )
  )

  # enable access to ... args in wrapper
  added_params <- new_environment(list(.name = name, ...), parent = parent.frame())
  added_params <- env_rename(added_params, name = glue("wrapped-{obj_address(added_params)}"))

  default_params <- fn_fmls(wrapper) %>%
    purrr::discard_at(c("cmd", "args"))

  env_coalesce(added_params, as.environment(default_params))
  set_env(w, added_params)
}
