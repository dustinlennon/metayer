#' @include config.R utils-generic.R
NULL

# initialize options ##########################################################

#' Reset options from config.yml
#' 
#' @keywords internal
#' @param r_config_active the config section to use
reset_options_from_conf <- function(
    r_config_active = Sys.getenv("R_CONFIG_ACTIVE", "default")) {

  # localize scope for subsequent call to config_get (yaml.load)
  opt <- config_get(
    "options",
    r_config_active = r_config_active
  )
  
  do.call(options, opt)
}

# initialize logging ##########################################################

#' Setup logging
#' 
#' @keywords internal
#' @param r_config_active the config section to use
#' @param home the user's home directory
#' @param max_bytes the max_bytes parameter passed to logger::appender_file
#' @param max_files the max_files parameter passed to logger::appender_file
#' @param create_directory a boolean, TRUE to create the directory
initialize_logging <- function(
    r_config_active = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    home = fs::path_home(),
    max_bytes = 1000000L,
    max_files = 7L,
    create_directory = TRUE) {

  logger_reset()

  logfile <- config_get("logger", "logfile", r_config_active = r_config_active)
  if (is_null(logfile)) {
    log_appender(
      appender_void
    )
  } else {
    logfile <- glue(logfile)

    if (create_directory) {
      fs::dir_create(
        fs::path_dir(logfile)
      )
    }

    log_appender(
      appender_file(
        logfile,
        max_bytes = max_bytes,
        max_files = max_files
      )
    )
  }

  threshold <- config_get("logger", "threshold", r_config_active = r_config_active) %||% "INFO"
  log_threshold(
    getExportedValue("logger", threshold)
  )

  log_layout(
    layout_glue_generator(
      format = config_get("logger", "format")
    )
  )

  secondary_appender <- config_get(
    "logger",
    "secondary_appender",
    r_config_active = r_config_active
  ) %||% appender_void
  
  log_appender(
    secondary_appender,
    index = 2
  )

  log_layout(
    layout_glue_generator(
      format = config_get("logger", "format", r_config_active = r_config_active)
    ),
    index = 2
  )

}

# onLoad ######################################################################
.onLoad <- function(libname, pkgname) {
  is_authoring <- isTRUE(getOption("knitr.in.progress")) ||
    isTRUE(getOption("jupyter.in_kernel"))

  reset_options_from_conf()
  initialize_logging()

  if (is_authoring) {
    knitr::opts_knit$set(out.format = "html")
  }
}
