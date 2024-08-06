#' An option set for cli text output
cli_test_opts <- function() {
  num_colors <- 1
  list(
    cli.dynamic = FALSE,
    cli.ansi = FALSE,
    cli.unicode = FALSE,
    crayon.enabled = num_colors > 1,
    crayon.colors = num_colors
  )
}

#' Set up logging for testing
logger_test_init <- function() {
  options(
    c(
      cli.default_handler = logged_cli_handler,
      cli_test_opts()
    )
  )

  log_layout(
    layout_glue_generator(
      format = "{ns} {level} : {msg}"
    )  
  )
}
