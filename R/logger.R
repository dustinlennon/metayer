app_factory <- function() {
  app <- cli::default_app() %||% cli::start_app(.auto_close = FALSE)

  #' A variant of cli:::clii__inline where we apply a user-specified transformer.
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

  app$inline <- set_env(app_inline, environment(app$inline))
  app
}


default_handler <- function(msg) {
  type <- as.character(msg$type)[1]
  app <- app_factory()
  do.call(app[[type]], msg$args)
}


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


logger_opts <- function() {
  list(
    cli.default_handler = getOption("cli.default_handler") %||% default_handler,
    metayer.transformer = app_transformer
  )
}

log_wrapper <- function(
    handler,
    threshold,
    message,
    ...,
    collapse = TRUE,
    strip_newline = FALSE,
    .envir = parent.frame()) {  # nolint

  withr::local_options(logger_opts())

  # mv <- getOption("metayer.verbosity") %||% "unset"
  # msg <- stringr::str_glue("log_wrapper: metayer.verbosity: {mv}")
  # cat(msg, "\n", file = stderr())

  if (getOption("metayer.verbosity", default = 30) <= threshold) {
    cli::cli_fmt(
      handler(message, .envir = .envir),
      collapse = collapse,
      strip_newline = strip_newline
    ) %>%
      gsub("\n$", "", .)
  }
}

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

#' @export
log_alert_info <- function(text, id = NULL, class = NULL, wrap = FALSE, .envir = parent.frame()) {
  withr::local_options(logger_opts())
  cli::cli_alert_info(text, id = id, class = class, wrap = wrap, .envir = .envir)
}

#' @export
log_alert_success <- function(text, id = NULL, class = NULL, wrap = FALSE, .envir = parent.frame()) {
  withr::local_options(logger_opts())
  cli::cli_alert_success(text, id = id, class = class, wrap = wrap, .envir = .envir)
}

#' @export
log_alert_warning <- function(text, id = NULL, class = NULL, wrap = FALSE, .envir = parent.frame()) {
  withr::local_options(logger_opts())
  cli::cli_alert_warning(text, id = id, class = class, wrap = wrap, .envir = .envir)
}

#' @export
log_alert_danger <- function(text, id = NULL, class = NULL, wrap = FALSE, .envir = parent.frame()) {
  withr::local_options(logger_opts())
  cli::cli_alert_danger(text, id = id, class = class, wrap = wrap, .envir = .envir)
}
