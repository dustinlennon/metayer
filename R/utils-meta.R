#' @include utils-generic.R
NULL

#' Inject code
inject <- function(cmd, level, .caller_env = caller_env()) {
  namespace <- environmentName(topenv(.caller_env))
  namespace <- if (namespace == "R_GlobalEnv") {
    "global.cli" 
  } else {
    stringr::str_glue("{namespace}.cli")
  }

  withr::with_environment(
    env(
      .namespace = namespace,
      .level = level
    ),
    {
      cnd <- catch_cnd(cmd)
      if (inherits(cnd, "cli_message")) {
        handler <- getOption("cli.default_handler", metayer_cli_handler)
        handler(cnd)
      } else if (inherits(cnd, "condition")) {
        cli::cli_verbatim(
          format(cnd)
        )
        if (inherits(cnd, "error")) {
          rlang::abort(
            conditionMessage(cnd)
          )
        }
      }
    }
  )
}

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

  injection <- eval(
    call(
      "substitute",
      body(inject),
      env(
        cmd = cmd,
        level = level,
        .caller_env = caller_env()
      )
    )
  )

  wrapped <- function() {}
  formals(wrapped) <- fml
  body(wrapped) <- injection
  environment(wrapped) <- topenv()

  wrapped
}


#' Wrap an exported function, safely
#' 
#' @inheritParams wrap_factory
#' @returns a wrapped function
wrap_factory_safe <- function(pkg, name, level) {
  try_fetch(
    wrap_factory(pkg, name, level),
    error = function(cnd) {
      getExportedValue(pkg, name)
    }
  )
}

pkg_demo_cli <- function() {
  names <- env_stack(caller_env()) %>%
    purrr::map_vec(env_name)
  cli_ol(names)
}
