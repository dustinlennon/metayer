#' @include config.R utils-generic.R
NULL

#' Set environmental variables to default values if necessary
#' 
#' @keywords internal
set_envvar_defaults <- function() {
  if (is_na(Sys.getenv("R_HERE_HERE", NA))) {
    Sys.setenv(R_HERE_HERE = here::here())
  }
}

#' Reset options from config.yml
#' 
#' @keywords internal
reset_options_from_conf <- function() {
  opt <- config_get("options") %||% list()
  do.call(options, opt)
}

#' Reset pander options from config.yml
#' 
#' @keywords internal
reset_pander_from_conf <- function() {
  config_get("pander", "panderOptions") %||% list() %>%
    purrr::imap(\(v, k) pander::panderOptions(k, v))

  config_get("pander", "evalsOptions") %||% list() %>%
    purrr::imap(\(v, k) pander::evalsOptions(k, v))

}

#' Setup logging
#' 
#' @keywords internal
#' @param max_bytes the max_bytes parameter passed to logger::appender_file
#' @param max_files the max_files parameter passed to logger::appender_file
#' @param create_directory a boolean, TRUE to create the directory
initialize_logging <- function(
    max_bytes = 1000000L,
    max_files = 7L,
    create_directory = TRUE) {

  logger_reset()

  logfile <- config_get("logger", "logfile")

  primary_appender <- if (is_null(logfile)) {
    logger::appender_void
  } else {
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
  set_envvar_defaults()

  reset_options_from_conf()
  reset_pander_from_conf()

  initialize_logging()
}
