#' @include config.R
NULL

.onLoad <- function(libname, pkgname) {
  is_authoring <- isTRUE(getOption("knitr.in.progress")) ||
    isTRUE(getOption("jupyter.in_kernel"))

  # Set up some logging
  setup_logging()

  opt <- config_get("metayer")
  options(
    cli.default_handler = get(opt$cli.default_handler),
    mty.cli_null = opt$mty.cli_null,
    mty.hash_label_length = opt$mty.hash_label_length
  )

  if (is_authoring) {
    initialize_vignette()
  }
}

#' Setup logging
#' 
#' @param home the user's home directory
#' @param max_bytes the max_bytes parameter passed to logger::appender_file
#' @param max_files the max_files parameter passed to logger::appender_file
#' @param create_directory a boolean, TRUE to create the directory
#' @export
setup_logging <- function(
    home = fs::path_home(),
    max_bytes = 1000000L,
    max_files = 7L,
    create_directory = TRUE) {

  logfile <- config_get("logger", "template") %>%
    glue()

  threshold <- getExportedValue("logger", config_get("logger", "threshold"))

  if (create_directory) {
    fs::dir_create(
      fs::path_dir(logfile)
    )
  }

  appender <- if (config_get("logger", "tee") == TRUE) {
    logger::appender_tee
  } else {
    logger::appender_file
  }

  log_appender(
    appender(
      logfile,
      max_bytes = max_bytes,
      max_files = max_files
    )
  )

  log_layout(
    layout_glue_generator(
      format = config_get("logger", "format")
    )
  )

  log_threshold(
    threshold
  )

}
