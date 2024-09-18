#' Create the default CLI app object
#' 
#' keywords internal
#' @export
cli_app_factory <- function() {
  cli::default_app() %||% cli::start_app(.auto_close = FALSE)
}

#' logger appropriate cli options
#' 
#' @keywords internal
#' @returns a list of options
captured_cli_opts <- function() {
  list(
    cli.dynamic = FALSE,
    cli.ansi = FALSE,
    cli.unicode = FALSE,
    crayon.enabled = FALSE,
    crayon.colors = 1
  )
}

#' Capture text from cli message
#' 
#' @param msg a cli_message
#' @export
capture_cli_message <- function(msg) {
  if (inherits(msg, "cli_message)")) {
    cli_abort("capture_cli_message: msg must inherit the cli_message class")
  }

  app <- purrr::pluck(msg, "args", "text", "values", "app") %||% cli_app_factory()
  type <- purrr::pluck(msg, "type")

  # If specified, replace NULL values with mty.cli_null
  cenv <- purrr::pluck(msg, "args", "text", "values")
  if (!is.null(cenv)) {
    cenv <- as.list(cenv) %>%
      purrr::modify2(
        .,
        names(.),
        function(v, k) {
          if (grepl("^v[0-9]+", k)) {
            v <- v %||% getOption("mty.cli_null")
          }
          v
        } 
      ) %>%
      new_environment()
    purrr::pluck(msg, "args", "text", "values") <- cenv
  }

  # have cliapp output to a raw connection
  output <- local({
    raw_output <- rawConnection(raw(1000), open = "wb")
    on.exit(close(raw_output))

    withr::with_options(
      c(
        captured_cli_opts()
      ),
      {
        old_output <- app$output
        old_signal <- app$signal
        tryCatch(
          {
            app$output <- raw_output
            app$signal <- FALSE
            do.call(app[[type]], msg$args)
          },
          finally = {
            app$output <- old_output
            app$signal <- old_signal
          }
        )
      }
    )

    rawToChar(rawConnectionValue(raw_output))
  })

  output
}
