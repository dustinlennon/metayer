.onLoad <- function(libname, pkgname) {
  is_authoring <- isTRUE(getOption("knitr.in.progress")) ||
    isTRUE(getOption("jupyter.in_kernel"))

  # Use R_CONFIG_ACTIVE to set config
  cfg <- config::get("logger")

  # Set up some logging
  setup_logging(
    cfg$template,
    tee = cfg$tee,
  )

  opt <- config::get("metayer_options")
  options(
    cli.default_handler = bang_expr(
      opt$cli.default_handler
    ),
    metayer.cli_null = opt$metayer.cli_null,
    metayer.hash_label_length = opt$metayer.hash_label_length
  )

  if (is_authoring) {
    initialize_vignette()
  }
}

#' Setup logging
#' 
#' @param logfile_template a glue template
#' @param home the user's home directory
#' @param user the user's login name
#' @param max_bytes the max_bytes parameter passed to logger::appender_file
#' @param max_files the max_files parameter passed to logger::appender_file
#' @param create_directory a boolean, TRUE to create the directory
#' @param tee a boolean, TRUE sends to file and stdout; FALSE, only file
#' @export
setup_logging <- function(
    logfile_template,
    home = fs::path_home(),
    user = Sys.info()[["login"]],
    max_bytes = 1000000L,
    max_files = 7L,
    create_directory = TRUE,
    tee = TRUE) {

  handler <- function(cnd)  {
    cmsg <<- conditionMessage(cnd) %>%
      cli::ansi_strip() %>%
      gsub("\n", " \\\\ ", .)

    msg <- glue::glue(
      "setup_logging:  {cmsg}"
    )

    rlang::warn(msg)
  }


  tryCatch(
    {
      logfile <- glue::glue(logfile_template)

      if (create_directory) {
        fs::dir_create(
          fs::path_dir(logfile)
        )
      }

      appender <- if (tee == TRUE) {
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
          format = "{pid} {ns} {level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}"
        )
      )

      log_info("initialized logger")
      
      `R.utils`::printf(
        ">>> initialized logger: %s\n", logfile,
        file = stderr()
      )
    },
    error = handler,
    warning = handler
  )
}
