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

#' A logged abort
#' 
#' @param message a glue-able string
#' @param .envir the environment in which to evaluate the message
#' @export
mty_abort <- function(
    message,
    .envir = parent.frame()) {

  message <- try_fetch(
    {
      glue::glue(
        message,
        .null = "<null>",
        .envir = .envir
      )
    },
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      sprintf("mty_abort error processing '{message}': {msg}")
    }

  )

  message %T>%
    log_error() %>%
    rlang::abort()
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
#' Set the "hash_label_length" option to change the default (identity)
#' 
#' @param val the (hash) value to trim
#' @export
hash_trim <- function(val) {
  l <- getOption("metayer.hash_label_length") %||% nchar(val)
  val %>%
    stringr::str_sub(-l, -1)
}


#' Parse and eval a string
#' 
#' @param s the string
#' @param par_env the environment in which to evaluate the string
#' @keywords internal
#' @export
bang_expr <- function(s, par_env = parent.frame()) {
  eval(parse(text = s), envir = par_env)
}