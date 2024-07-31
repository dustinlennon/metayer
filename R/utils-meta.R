#' @include utils-generic.R
NULL

#' Wrap an exported function
#' 
#' @param pkg the package name
#' @param name the object (function) name
#' @param level the logger level
#' @returns a wrapped function
wrap_factory <- function(pkg, name, level) {

  f <- getExportedValue(pkg, name)
  fml <- formals(f)

  w_args <- names(fml) %>%
    set_names() %>%
    purrr::map(
      \(o) str2lang(o)
    ) %>%
    as.pairlist()

  qual_name <- str2lang(stringr::str_glue("{pkg}::{name}"))
  cmd <- as.call(c(qual_name, w_args))

  # TODO - can we use the catch_cnd and cli::verbatim?
  code <- substitute(
    {
      namespace <- environmentName(caller_env())
      namespace <- if (namespace == "R_GlobalEnv") "global" else namespace

      withr::with_environment(
        env(
          .namespace = namespace,
          .level = level
        ),
        cmd
      )
    },
    env = env(cmd = cmd, level = level)
  )

  wrapped <- function() {}
  formals(wrapped) <- fml
  body(wrapped) <- do.call("call", list("{", code), quote = TRUE)
  # environment(wrapped) <- topenv()

  wrapped
}


#' Wrap an exported function, safely
#' 
#' @inheritParams wrap_factory
#' @returns a wrapped function
#' @export
wrap_factory_safe <- function(pkg, name, level) {
  try_fetch(
    wrap_factory(pkg, name, level),
    error = function(cnd) {
      getExportedValue(pkg, name)
    }
  )
}
