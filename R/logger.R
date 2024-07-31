# cliapp ----------------------------------------------------------------------

#' Create a CLI app object with custom behavior.
#' 
#' This monkey patches the behavior of the "inline" method so that it may
#' use a custom transformer, specified as an option.
#' 
#' @returns a cli app object
metayer_app_factory <- function(app = NULL) {
  app <- app %||% cli_app_factory()

  # a variant of cli:::clii__inline, allowing a custom transformer
  app_inline <- function(text = NULL, .list = NULL) {
    texts <- c(if (!is.null(text)) list(text), .list)

    out <- lapply(texts, function(t) {
      t$values$app <- app
      glue(
        t$str,
        .envir = t$values,
        .transformer = getOption("metayer.transformer", inline_transformer),
        .open = paste0("<", t$values$marker),
        .close = paste0(t$values$marker, ">"),
        .trim = FALSE
      )
    })
    paste(out, collapse = "")
  }

  # monkey patch the "inline" method
  app$inline <- set_env(app_inline, environment(app$inline))

  app
}

#' Create the default CLI app object
#' 
#' @export
cli_app_factory <- function() {
  cli::default_app() %||% cli::start_app(.auto_close = FALSE)
}

# message handlers ------------------------------------------------------------

#' A CLI message handler
#' 
#' This is a variant of cli::cli_server_default_safe
#' 
#' @param msg a cli_message
#' @export
metayer_cli_handler <- function(msg) {
  type <- as.character(msg$type)[1]
  app <- metayer_app_factory()
  do.call(app[[type]], msg$args)
}

captured_cli_opts <- function() {
  list(
    cli.dynamic = FALSE,
    cli.ansi = FALSE,
    cli.unicode = FALSE,
    crayon.enabled = FALSE,
    crayon.colors = 1
  )
}

#' Capture CLI messages and log
#' 
#' @inheritParams metayer_cli_handler
#' @export
logged_cli_handler <- function(msg) {
  level <- env_get(nm = ".level", default = logger::INFO, inherit = TRUE)
  namespace <- env_get(nm = ".namespace", default = "global", inherit = TRUE)

  # some trace logging
  m <- stringr::str_glue(
    "
    logged_cli_handler: level = {level}; namespace = {namespace}
    "
  )
  logger::log_trace(m, namespace = "metayer")

  type <- as.character(msg$type)[1]
  app <- metayer_app_factory()

  # Ref. cli:::cli__fmt
  app <- app %||% default_app() %||% start_app(.auto_close = FALSE)
  old <- app$output
  oldsig <- app$signal
  on.exit(app$output <- old, add = TRUE)
  on.exit(app$signal <- oldsig, add = TRUE)
  out <- rawConnection(raw(1000), open = "wb")
  on.exit(close(out), add = TRUE)
  app$output <- out
  app$signal <- FALSE

  withr::with_options(
    captured_cli_opts(),
    do.call(app[[type]], msg$args)
  )

  txt <- rawToChar(rawConnectionValue(out)) %>%
    cli::ansi_strip()

  txt <- sub("\n$", "", txt)

  logger::log_level(level, txt)
}

#' A null CLI message handler
#' 
#' @inheritParams metayer_cli_handler
null_cli_handler <- function(msg) {}

#' The original cli handler
#' 
#' @inheritParams metayer_cli_handler
default_cli_handler <- function(msg) {
  type <- as.character(msg$type)[1]
  app <- cli::default_app() %||% cli::start_app(.auto_close = FALSE)
  do.call(app[[type]], msg$args)
}


# transformers ----------------------------------------------------------------

#' A NULL aware alternative to cli:::inline_transformer
#' 
#' @param code The text inside the "..." glue substitution
#' @param envir Environment with the data to perform the styling.
#' @returns the output of cli:::inline_transformer after a "NULL" substitution
#' @export
null_aware_transformer <- function(code, envir) {

  if (!env_has(envir, "app")) {
    envir <- env_clone(envir)
    app <- metayer_app_factory()
    envir[["app"]] <- app
  }

  try_fetch(
    {
      expr <- parse(text = code, keep.source = FALSE)
      val <- eval(expr, envir = envir)
      if (is.null(val)) {
        envir[[code]] <- ".NULL."
      }
    },
    error = function(cnd) NULL
  )

  withr::with_namespace(
    "cli",
    inline_transformer(code, envir)
  )
}

# # cli_* functions -------------------------------------------------------------

# #' Emit a log at 'debug' level
# #' 
# #' @param message the message
# #' @param ... passed through to the wrapped function
# #' @param .class the class of the error
# #' @param .parent the parent of the error
# #' @param .envir the environment in which to evaluate the message
# #' @export
# cli_debug <- function(
#     message,
#     ...,
#     .class = NULL,
#     .parent = NULL,
#     .envir = parent.frame()) {

#   cnd <- catch_cnd(
#     cli::cli_inform(
#       message,
#       ...,
#       call = .envir,
#       .envir = .envir,
#       class = .class,
#       parent = .parent
#     )
#   )

#   withr::with_environment(
#     env(.log_level = logger::DEBUG),
#     cli::cli_verbatim(conditionMessage(cnd))
#   )

#   invisible()
# }
  

# #' Emit a log at 'inform' level
# #' 
# #' @inheritParams cli_debug
# #' @export
# cli_info <- function(
#     message,
#     ...,
#     .class = NULL,
#     .parent = NULL,
#     .envir = parent.frame()) {

#   cnd <- catch_cnd(
#     cli::cli_inform(
#       message,
#       ...,
#       call = .envir,
#       .envir = .envir,
#       class = .class,
#       parent = .parent
#     )
#   )

#   withr::with_environment(
#     env(.log_level = logger::INFO),
#     cli::cli_verbatim(conditionMessage(cnd))
#   )

#   invisible()
# }

# #' Emit a log at 'warn' level
# #' 
# #' @inheritParams cli_debug
# #' @export
# cli_warn <- function(
#     message,
#     ...,
#     .class = NULL,
#     .parent = NULL,
#     .envir = parent.frame()) {

#   cnd <- catch_cnd(
#     cli::cli_warn(
#       message,
#       ...,
#       call = .envir,
#       .envir = .envir,
#       class = .class,
#       parent = .parent
#     )
#   )

#   withr::with_environment(
#     env(.log_level = logger::WARN),
#     cli::cli_verbatim(conditionMessage(cnd))
#   )

#   invisible()
# }

# #' Calls cli_abort
# #' 
# #' @inheritParams cli_debug
# #' @export
# cli_abort <- function(
#     message,
#     ...,
#     .class = NULL,
#     .parent = NULL,
#     .envir = parent.frame()) {

#   cnd <- catch_cnd(
#     cli::cli_abort(
#       message,
#       ...,
#       call = .envir,
#       .envir = .envir,
#       class = .class,
#       parent = .parent
#     )
#   ) 

#   withr::with_environment(
#     env(.log_level = logger::ERROR),
#     cli::cli_verbatim(
#       format(cnd, simplify = "branch")
#     )
#   )

#   .class <- .class %||% "error"
#   rlang::abort(class = .class, parent = cnd, .frame = parent.frame())

#   invisible()
# }


# # if (FALSE) {
# #   cnd <- rlang::catch_cnd(
# #     rlang::abort("foo!", class = "zaa")
# #   )
# # }