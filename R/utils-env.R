#' Rename an environment
#' 
#' @param e the environment
#' @param name the new name, to be passed to str_glue
#' @param envir the environment to pass to str_glue
#' @returns the environment with the 'name' attribute set
#' @export
env_rename <- function(e, name, envir = parent.frame()) {
  name <- stringr::str_glue(name, envir = envir)
  e %>%
    magrittr::set_attr("name", name)
}

#' Return the environment stack
#' 
#' Utility function that wraps env_parents.  However, it always includes the current environment
#' and will handle function environments.
#' 
#' @param e the environment; or function
#' @param last passed to the underlying call to env_parents
#' @returns a list of environments
#' @export
env_stack <- function(
    e,
    last = getOption("pkgette.env_stack_last")) {

  if (is.function(e)) {
    e <- environment(e)
  }

  new_environment(parent = e) %>%
    env_parents(last = last)
}

