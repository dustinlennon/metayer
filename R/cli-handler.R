# #' @include utils-cli.R
# NULL

#' Create the default CLI app object
#' 
#' keywords internal
#' @export
cli_app_factory <- function() {
  cli::default_app() %||% cli::start_app(.auto_close = FALSE)
}

#' Handle CLI messages
#' 
#' @param msg a cli_message
#' @export
cli_metayer_handler <- function(msg) {
  app <- cli_app_factory()
  do.call(app[[msg$type]], msg$args)
  invisible()
}

#' Let cli methods replace NULL values with sensible substitutions
#' 
#' @inheritParams cli_metayer_handler
#' @export
cli_nullity_handler <- function(msg) {
  mv_env <- purrr::chuck(msg, "args", "text", "values") %>%
    as.list() %>%
    purrr::imap(
      function(v, k) {
        if (grepl("^v[0-9]+$", k) && is_null(v)) {
          getOption("mty.cli_null")
        } else {
          v
        }
      }
    ) %>%
    new_environment()

  purrr::pluck(msg, "args", "text", "values") <- mv_env

  app <- cli_app_factory()
  do.call(app[[msg$type]], msg$args)
  invisible()
}
