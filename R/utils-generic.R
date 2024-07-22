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


#' Raise an error for not yet implemented functions
#' 
#' @param is_terminal if TRUE, abort; if FALSE, warn
not_yet_implemented <- function(is_terminal = TRUE) {
  sp <- sys.parent()

  mc <- match.call(
    definition = sys.function(sp),
    call = sys.call(sp)
  )

  msg <- stringr::str_glue("'{mc[[1]]}' is not yet implemented")

  if (is_terminal) {
    abort(msg, "not-yet-implemented")
  } else {
    warn(msg, "not-yet-implemented")
  }

  invisible(NULL)
}

#' Raise an error for deprecated functions
#' 
#' @param is_terminal if TRUE, abort; if FALSE, warn
deprecated <- function(is_terminal = TRUE) {
  sp <- sys.parent()

  mc <- match.call(
    definition = sys.function(sp),
    call = sys.call(sp)
  )

  msg <- stringr::str_glue("'{mc[[1]]}' is deprecated")

  if (is_terminal) {
    abort(msg, "deprecated")
  } else {
    warn(msg, "deprecated")
  }

  invisible(NULL)
}

#' Detect devtools shims
#' 
#' @returns TRUE if shimmed; else, FALSE
is_shimmed <- function() {
  sfe <- env_name(environment(system.file))
  if (sfe == "namespace:base") {
    FALSE
  } else if (sfe == "namespace:pkgload") {
    TRUE
  } else {
    log_abort("unknown environmental configuration for system.file: '{sfe}'")
  }
}

#' Trim a hash value
#' 
#' Set the "hash_label_length" option to change the default (identity)
#' 
#' @param val the (hash) value to trim
#' @export
hash_trim <- function(val) {
  l <- config::get("hash_label_length") %||% nchar(val)
  val %>%
    stringr::str_sub(-l, -1)
}
