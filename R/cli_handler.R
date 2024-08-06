#' Create the default CLI app object
#' 
#' @export
cli_app_factory <- function() {
  cli::default_app() %||% cli::start_app(.auto_close = FALSE)
}

# message handlers ------------------------------------------------------------

#' Preprocess a message
#' 
#' Change NULL values, if metayer.cli.null option is set
#' @param msg a cli message
#' @returns the message type
preprocess_msg <- function(msg) {

  # Rewrite NULL values in post (undocumented functionality)
  cli_null <- getOption("metayer.cli.null")
  if (!is.null(cli_null)) {
    venv <- msg$args$text$values
    vnames <- grep("^v\\d+", names(venv), value = TRUE)
    for (key in vnames) {
      if (is.null(venv[[key]]))
        venv[[key]] <- cli_null
    }
  }

  as.character(msg$type)[1]  
}

#' Handle CLI messages
#' 
#' @param msg a cli_message
#' @export
metayer_cli_handler <- function(msg) {
  # a per-call identifier
  uuid <- uuid::UUIDgenerate() %>% 
    hash() %>%
    stringr::str_sub(-4, -1)

  type <- preprocess_msg(msg)

  log_trace("{uuid} metayer_cli_handler")
  log_trace("{uuid} type = {type}")

  app <- cli_app_factory()
  do.call(app[[type]], msg$args)
  invisible()
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

#' Redirect CLI messages to logger
#' 
#' @inheritParams metayer_cli_handler
#' @export
logged_cli_handler <- function(msg) {
  # a per-call identifier
  uuid <- uuid::UUIDgenerate() %>% 
    hash() %>%
    stringr::str_sub(-4, -1)

  # metadata passed through via modified execution stack
  level <- env_get(nm = ".log_level", default = logger::TRACE, inherit = TRUE)
  namespace <- env_get(nm = ".log_namespace", default = "xxx", inherit = TRUE)

  if (!namespace %in% logger::log_namespaces()) {
    log_info("adding logger namespace: {namespace}")
    log_formatter(formatter_paste, namespace = namespace)
  }

  type <- preprocess_msg(msg)

  log_trace("{uuid} logged_cli_handler begin")
  log_trace("{uuid} level = {level}; namespace = {namespace}; type = {type}")

  app <- cli_app_factory()

  # Ref. cli:::cli__fmt
  old <- app$output
  oldsig <- app$signal
  on.exit(app$output <- old, add = TRUE)
  on.exit(app$signal <- oldsig, add = TRUE)
  out <- rawConnection(raw(1000), open = "wb")
  on.exit(close(out), add = TRUE)
  app$output <- out
  app$signal <- FALSE

  status <- withr::with_options(
    captured_cli_opts(),
    do.call(app[[type]], msg$args)
  )

  txt <- rawToChar(rawConnectionValue(out)) %>%
    cli::ansi_strip()
  txt <- sub("\n$", "", txt)

  if (nchar(txt) > 0) {
    log_trace("{uuid} handler generated output")
    logger::log_level(level, txt, namespace = namespace)
  } else {
    log_trace("{uuid} handler was silent")
  }

  log_trace("{uuid} logged_cli_handler end")

  invisible(status)
}
