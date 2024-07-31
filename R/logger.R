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

  # Rewrite NULL values in post
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
  # a per-call identifier
  uuid <- uuid::UUIDgenerate() %>% 
    hash() %>%
    stringr::str_sub(-4, -1)

  # metadata passed through via modified execution stack
  level <- env_get(nm = ".level", default = logger::INFO, inherit = TRUE)
  namespace <- env_get(nm = ".namespace", default = "xxx", inherit = TRUE)

  type <- preprocess_msg(msg)

  log_trace("{uuid} logged_cli_handler")
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

  status
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