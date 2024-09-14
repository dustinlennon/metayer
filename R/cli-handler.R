#' @include utils-cli.R
NULL

#' Handle CLI messages
#' 
#' @param msg a cli_message
#' @export
metayer_cli_handler <- function(msg) {
  app <- cli_app_factory()
  do.call(app[[type]], msg$args)
  invisible()
}

#' Redirect CLI messages to logger
#' 
#' @inheritParams metayer_cli_handler
#' @export
logged_cli_handler <- function(msg) {
  # log_info("> logged_cli_handler")

  # set metadata, if available
  metadata <- purrr::pluck(msg, "args", ".envir", "metadata") %||% list()
  level <- metadata$level %||% logger::INFO
  namespace <- metadata$namespace %||% "unknown"

  if (!namespace %in% logger::log_namespaces()) {
    log_info("adding logger namespace: {namespace}")
    log_formatter(formatter_paste, namespace = namespace)
  }

  cnd_text <- capture_cli_message(msg)
  txt <- sub("\n$", "", cnd_text)
  if (nchar(txt) > 0) {
    logger::log_level(level, txt, namespace = namespace)
  }

  # reraise warnings and errors
  cnd <- purrr::pluck(msg, "args", ".envir", "cnd")
  if (inherits(cnd, c("warning", "error"))) {
    cnd_signal(cnd)
  }

  invisible(NULL)
}
