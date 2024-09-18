#' Utility function that resets the global environment.
#' 
#' @param exclusions a list of object names to be excluded from the removal.
#' @export
rm.all <- function(exclusions = c()) { # nolint
  exclusions <- c("workflow", exclusions)

  all_names <- global_env() %>%
    names()

  rm(
    list = setdiff(all_names, exclusions),
    envir = global_env()
  )
}

#' Detect devtools shims
#' 
#' @keywords internal
#' @returns TRUE if shimmed; else, FALSE
is_shimmed <- function() {
  sfe <- env_name(environment(system.file))
  if (sfe == "namespace:base") {
    FALSE
  } else if (sfe == "namespace:pkgload") {
    TRUE
  } else {
    cli_abort("unknown environmental configuration for system.file: '{sfe}'")
  }
}

#' Trim a hash value
#' 
#' Set the "mty.hash_label_length" option to change the default (identity)
#' 
#' @param val the (hash) value to trim
#' @export
hash_trim <- function(val) {
  l <- getOption("mty.hash_label_length") %||% nchar(val)
  val %>%
    stringr::str_sub(-l, -1)
}

#' Recursively update a list
#' 
#' @param x the destination
#' @param y the update
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

#' Get a uuid
#' 
#' @param ... pass through parameters
#' @export
mty_uuid <- function(...) {
  uuid_generator <- getOption("uuid.generator", default = uuid::UUIDgenerate)
  uuid_generator(...)
}

#' Create a predictable identifier sequence
#' 
#' This is probably most useful when testing, as one can set the corresponding entry
#' in the config.yml to get reproducible results.
#' 
#' @export
mty_salted_hash <- function(salt = NULL) {
  salt <- salt %||% getOption("uuid.salt", "undefined")
  result <- hash(salt)
  options(uuid.salt = result)

  sprintf(
    "%s-%s-%s-%s-%s",
    stringr::str_sub(result, 1, 8),
    stringr::str_sub(result, 1, 4),
    stringr::str_sub(result, 1, 4),
    stringr::str_sub(result, 1, 4),
    stringr::str_sub(result, 1, 12)
  )
}
