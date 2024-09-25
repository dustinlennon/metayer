#' an improved config::get
#' 
#' config_get behaves much like config::get.  However, it improves upon its predecessor in the
#' following ways: 
#' + handlers for custom YAML tags
#' + allows YAML references
#' 
#' @param ... passed to purrr::pluck to extract components within the active config
#' @param config the configuration; defaults to R_CONFIG_ACTIVE
#' @param file the yaml file; defaults to config.yml
#' @export
config_get <- function(
    ...,
    r_config_active = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    file = Sys.getenv("R_CONFIG_FILE", here::here("config.yml")),
    merge.precedence = "override",
    handlers = list(
      optenv = yaml_optenv_handler,
      with_env = yaml_withenv_handler
    )) {
  
  file <- normalizePath(file, mustWork = FALSE)
  config_yaml <- yaml::read_yaml(
    file,
    merge.precedence = merge.precedence,
    handlers = handlers
  )

  purrr::pluck(config_yaml, r_config_active, ...)
}
