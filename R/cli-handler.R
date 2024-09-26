# #' @include utils-cli.R
# NULL

#' obtain a CLI instance
#' 
#' keywords internal
#' @export
cli_app_factory <- function() {
  cli::default_app() %||% cli::start_app(.auto_close = FALSE)
}

#' replace NULLs with visible values
#' 
#' @param msg a cli-message object
#' @export
cli_nullity_handler <- function(msg) {
  mv_env <- purrr::pluck(msg, "args", "text", "values")
  
  if (!is_null(mv_env)) {
    mv_env <- as.list(mv_env) %>%
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
  }

  # This is equivalent to the default handler, c.f.
  # cli:::cli_server_default_safe
  type <- as.character(msg$type)[1]
  app <- cli_app_factory()
  do.call(app[[type]], msg$args)
  invisible()
}
