#' an improved config::get
#' 
#' config_get behaves much like config::get.  However, it improves upon its predecessor in the
#' following ways: 
#' + handlers for custom YAML tags
#' + allows YAML references
#' 
#' @param ... passed to purrr::pluck to extract components within the active config
#' @param r_config_active the configuration; defaults to R_CONFIG_ACTIVE
#' @param file the config file; defaults to config.yml
#' @param merge.precedence merge.precedence; defaults to "override" for YAML references
#' @param handlers a list of handlers for YAML tags
#' @export
config_get <- function(
    ...,
    r_config_active = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    file = Sys.getenv("R_CONFIG_FILE", here::here("config.yml")),
    merge.precedence = "override",
    handlers = list(
      optenv = yaml_handler_optenv,
      with_env = yaml_handler_with_env
    )) {
  
  file <- normalizePath(file, mustWork = FALSE)

  tryCatch(
    {
      withr::with_options(
        list(
          rlang_backtrace_on_error = "none"
        ),
        {
          config_yaml <- yaml::read_yaml(
            file,
            merge.precedence = merge.precedence,
            handlers = handlers
          )
        }
      )
    }, 
    warning = function(msg) {
      message <- "promoting yaml warning to error: {conditionMessage(msg)}"
      cli_abort(message)
    }
  )

  purrr::pluck(config_yaml, r_config_active, ...)
}
