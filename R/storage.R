#' Retrieve a storage environment
#'
#' @param ... an indexing collection of character objects
#' @param .store the name of the storage
#' @param .envir the environment into which to bind the storage data structure
#' @returns the specified environment
#' @export
storage_env <- function(
    ...,
    .store = ".storage",
    .envir = rlang::global_env()) {

  base_env <- env_cache(
    .envir,
    .store,
    new_environment() %>%
      env_rename(.store)
  ) 

  keys <- list(...) %>%
    unlist()

  labels <- purrr::accumulate(
    keys,
    \(x, y) stringr::str_flatten(c(x, y), collapse = ".")
  ) %>%
    purrr::map_chr(
      \(label) stringr::str_flatten(c(.store, label), collapse = ".")
    )

  iter_env <- base_env
  for (i in seq_along(keys)) {
    iter_env <- env_cache(
      iter_env,
      keys[i],
      new_environment()  %>% 
        env_rename(labels[i])
    )
  }

  iter_env
}

#' Purge the storage
#' 
#' @param .store the name of the storage
#' @param envir the environment into which to purge the storage_env data structure
#' @export
storage_purge <- function(.store = ".storage", .envir = rlang::global_env()) {
  env_unbind(
    .envir,
    .store
  )
}
