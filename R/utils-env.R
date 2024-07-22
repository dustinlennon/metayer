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

# #' Return the names in the environment stack 
# #' 
# #' Note that this removes hash suffixes.
# #' @param f an the environment; or function
# #' @returns a character vector of the environment names in the stack
# env_strip <- function(f) {
#   f %>%
#     env_stack() %>%
#     purrr::map_chr(env_name) %>%
#     unname() %>%
#     purrr::map_chr(
#       \(s) sub("_[0-9a-f]{4}$", "", s)
#     )
# }


# #' Merge environments
# #' 
# #' Merge src into dest.  Src will overwrite elements in dest.
# #' 
# #' @param dest the destination environment
# #' @param src the source environment
# env_merge <- function(
#     dest,
#     src) {

#   src_names <- names(src)
#   env_unbind(dest, src_names)
#   env_coalesce(dest, src)

#   invisible(NULL)
# }


