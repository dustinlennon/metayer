#' @include config.R utils-generic.R
NULL

#' Reset options from config.yml
#' 
#' @keywords internal
reset_options_from_conf <- function() {
  opt <- config_get("options") %||% list()
  do.call(options, opt)
}

#' Setup logging
#' 
#' @keywords internal
#' @param home the user's home directory
#' @param max_bytes the max_bytes parameter passed to logger::appender_file
#' @param max_files the max_files parameter passed to logger::appender_file
#' @param create_directory a boolean, TRUE to create the directory
initialize_logging <- function(
    home = fs::path_home(),
    max_bytes = 1000000L,
    max_files = 7L,
    create_directory = TRUE) {

  logger_reset()

  logfile <- config_get("logger", "logfile")

  primary_appender <- if (is_null(logfile)) {
    logger::appender_void
  } else {
    logfile <- glue(logfile)

    if (create_directory) {
      fs::dir_create(
        fs::path_dir(logfile)
      )
    }

    logger::appender_file(
      logfile,
      max_bytes = max_bytes,
      max_files = max_files
    )
  }

  threshold <- config_get("logger", "threshold") %||% logger::INFO

  layout <- logger::layout_glue_generator(
    format = config_get("logger", "format")
  )

  logger::log_appender(primary_appender)
  logger::log_layout(layout)
  logger::log_threshold(threshold)

  appenders <- config_get(
    "logger",
    "appenders"
  ) %||% list()

  for (i in seq_along(appenders)) {
    appender <- appenders[[i]]

    logger::log_appender(appender, index = i + 1)
    logger::log_layout(layout, index = i + 1)
    logger::log_threshold(threshold, index = i + 1)
  }

}

.metayer <- function() { 
  reset_options_from_conf()
  initialize_logging()
  log_debug("metayer package initialized")
}
