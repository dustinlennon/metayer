#' Utility function that resets the global environment.
#' 
#' @param exclusions a list of object names to be excluded from the removal.
#' @export
rm.all <- function(exclusions = c()) { # nolint
  
  exclusions <- c(
    exclusions,
    config_get("rmall_exclusions") %||%
      as.character()
  )

  all_names <- global_env() %>%
    names()

  rm(
    list = setdiff(all_names, exclusions),
    envir = global_env()
  )
}

#' recursively update a list
#' 
#' The result combines the original list with a refresh list, where, for any shared key, the result contains the value 
#' from the refresh list.
#' @param x the original nested list
#' @param y the refresh nested list
#' @returns a new list containing the update of original and refresh
#' @export 
update_list <- function(x, y) {
  if (!(is_list(x) && is_list(y))) {
    stop("both 'x' and 'y' should be lists")
  }

  nx <- names(x)
  ny <- names(y)
  new_keys <- setdiff(ny, nx)
  for (k in new_keys) {
    x[[k]] <- y[[k]]
  }

  common_keys <- intersect(nx, ny)
  for (k in common_keys) {
    v <- y[[k]]
    if (!is_list(v)) {
      x[[k]] <- v
    } else {
      x[[k]] <- update_list(x[[k]], v)
    }
  }

  return(x)
}

#' get a uuid
#' 
#' This can be adapted to produce seeded results by specifying the uuid.generator option, e.g.
#' options(uuid.generator = test_mty_uuid)
#' @param ... pass through parameters
#' @export
mty_uuid <- function(...) {
  uuid_generator <- getOption("uuid.generator", default = uuid::UUIDgenerate)
  uuid_generator(...)
}
