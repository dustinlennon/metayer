#' Create a CLI app object with custom behavior.
#' 
#' This monkey patches the behavior of the "inline" method so that it may
#' use a custom transformer, specified as an option.
#' 
#' @returns a cli app object
app_factory <- function() {
  app <- cli::default_app() %||% cli::start_app(.auto_close = FALSE)

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

#' A CLI message handler
#' 
#' This is a variant of cli::cli_server_default_safe
#' 
#' @param msg a cli_message
default_handler <- function(msg) {
  type <- as.character(msg$type)[1]
  app <- app_factory()
  do.call(app[[type]], msg$args)
}

#' A NULL aware alternative to cli:::inline_transformer
#' 
#' @param code The text inside the "..." glue substitution
#' @param envir Environment with the data to perform the styling.
#' @returns the output of cli:::inline_transformer after a "NULL" substitution
app_transformer <- function(code, envir) {

  if (!env_has(envir, "app")) {
    envir <- env_clone(envir)
    envir[["app"]] <- app_factory()
  }

  try_fetch(
    {
      expr <- parse(text = code, keep.source = FALSE)
      val <- eval(expr, envir = envir)
      if (is.null(val)) {
        envir[[code]] <- "NULL"
      }
    }, error = function(cnd) NULL
  )

  withr::with_namespace(
    "cli",
    inline_transformer(code, envir)
  )
}

#' Common logger options
logger_opts <- function() {
  list(
    cli.default_handler = getOption("cli.default_handler") %||% default_handler,
    metayer.transformer = app_transformer
  )
}

#' Common code shared by the other log_* functions
#' 
#' This wraps cli::cli_fmt with options that enable the NULL-aware logging
#' 
#' @param handler handler, to pass to cli::cli_fmt
#' @param threshold a logging emit threshold
#' @param message the user provided message
#' @param ... to be passed through to cli::cli_fmt
#' @param collapse to be passed through to cli::cli_fmt
#' @param strip_newline to be passed through to cli::cli_fmt
#' @param .envir the environment in which to evaluate the underlying glue function
log_wrapper <- function(
    handler,
    threshold,
    message,
    ...,
    collapse = TRUE,
    strip_newline = FALSE,
    .envir = parent.frame()) {  # nolint

  withr::local_options(logger_opts())

  if (getOption("metayer.verbosity", default = 30) <= threshold) {
    cli::cli_fmt(
      handler(message, .envir = .envir),
      collapse = collapse,
      strip_newline = strip_newline
    ) %>%
      gsub("\n$", "", .)
  }
}

#' Produce a NULL-aware "inform" message
#' 
#' @param message the user provided message
#' @param ... to be passed through to cli::cli_bullets
#' @param .envir the environment in which to evaluate the underlying glue function
#' @export
log_inform <- function(message, ..., .envir = parent.frame()) { # nolint
  msg <- log_wrapper(
    cli::cli_bullets,
    threshold = 20,
    message,
    ...,
    .envir = .envir
  )

  if (!is.null(msg))
    rlang::inform(msg)
}

#' Produce a NULL-aware "warn" message
#' 
#' @inheritParams log_inform
#' @export
log_warn <- function(message, ..., .class = NULL, .envir = parent.frame()) { # nolint
  msg <- log_wrapper(
    cli::cli_bullets,
    threshold = 30,
    message,
    ...,
    .envir = .envir
  )

  if (!is.null(msg))
    rlang::warn(msg, class = .class)
}

#' Produce a NULL-aware "abort" message
#' 
#' @inheritParams log_inform
#' @param .class an error class
#' @param .parent a parent exception, if warranted
#' @export
log_abort <- function(
    message,
    ...,
    .class = NULL,
    .parent = NULL,
    .envir = parent.frame()) { # nolint

  # rlang::abort adds an "!", so remove any leading prefix symbol
  names2(message)[1] <- ""

  msg <- log_wrapper(
    cli::cli_bullets,
    threshold = 40,
    message,
    ...,
    .envir = .envir
  )

  if (!is.null(msg))
    rlang::abort(
      msg,
      parent = .parent,
      class = .class,
      call = .envir,
      .frame = .envir
    )
}

#' Generate a NULL-aware cli_alert_info
#' 
#' @param text the user provided text
#' @param id id, to be passed through to cli::cli_alert_*
#' @param class class, to be passed through to cli::cli_alert_*
#' @param wrap wrap, to be passed through to cli::cli_alert_*
#' @param .envir the environment in which to evaluate the underlying glue function
#' @export
log_alert_info <- function(text, id = NULL, class = NULL, wrap = FALSE, .envir = parent.frame()) {
  withr::local_options(logger_opts())
  cli::cli_alert_info(text, id = id, class = class, wrap = wrap, .envir = .envir)
}

#' Generate a NULL-aware cli_alert_success
#' 
#' @inheritParams log_alert_info
#' @export
log_alert_success <- function(text, id = NULL, class = NULL, wrap = FALSE, .envir = parent.frame()) {
  withr::local_options(logger_opts())
  cli::cli_alert_success(text, id = id, class = class, wrap = wrap, .envir = .envir)
}

#' Generate a NULL-aware cli_alert_warning
#' 
#' @inheritParams log_alert_info
#' @export
log_alert_warning <- function(text, id = NULL, class = NULL, wrap = FALSE, .envir = parent.frame()) {
  withr::local_options(logger_opts())
  cli::cli_alert_warning(text, id = id, class = class, wrap = wrap, .envir = .envir)
}

#' Generate a NULL-aware cli_alert_danger
#' 
#' @inheritParams log_alert_info
#' @export
log_alert_danger <- function(text, id = NULL, class = NULL, wrap = FALSE, .envir = parent.frame()) {
  withr::local_options(logger_opts())
  cli::cli_alert_danger(text, id = id, class = class, wrap = wrap, .envir = .envir)
}
