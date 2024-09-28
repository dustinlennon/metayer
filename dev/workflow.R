#' Initialize workflow
#' 
#' This function is protected from rm.all.
#' @param r_config_acive R_CONFIG_ACTIVE environment variable
#' @param envir an environment in which to bind a self-reference
workflow <- function(r_config_active = NULL, envir = parent.frame()) {
  tryCatch(
    {
      cfg <- yaml::read_yaml(
        here::here("dev/workflow.yml")
      )

      setwd(
        cfg$base_dir
      )

      .libPaths(
        here::here("library")
      )

      if (rlang::is_null(r_config_active)) {
        Sys.unsetenv("R_CONFIG_ACTIVE")
      } else {
        Sys.setenv(R_CONFIG_ACTIVE = r_config_active)
      }

      devtools::load_all()

      rlang::env_bind(
        envir,
        workflow = sys.function()
      )
    },
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      cli::cli_alert_warning(msg)
    }
  )
}
