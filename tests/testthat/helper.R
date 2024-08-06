#' Set up logging for testing
logger_test_init <- function() {
  options(
    c(
      cli.default_handler = logged_cli_handler,
      captured_cli_opts()
    )
  )

  log_layout(
    layout_glue_generator(
      format = "{ns} {level} : {msg}"
    )  
  )
}
