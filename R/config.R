#' A variation of config::get that can utilize backreferences
#' 
#' @param ... passed to purrr::pluck
#' @param config the configuration; defaults to R_CONFIG_ACTIVE
#' @param file the yaml file; defaults to config.yml
#' @export
config_get <- function(
    ...,
    r_config_active = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    file = Sys.getenv("R_CONFIG_FILE", here::here("config.yml")),
    merge.precedence = "override",
    handlers = list(optenv = config_optenv_handler)) {
  
  file <- normalizePath(file, mustWork = FALSE)
  config_yaml <- yaml::read_yaml(
    file,
    merge.precedence = merge.precedence,
    handlers = handlers
  )

  purrr::pluck(config_yaml, r_config_active, ...)
}

config_optenv_handler <- function(obj) {
  withr::with_environment(
    current_env(),
    {
      expr <- parse(text = obj)
      eval(expr)
    }
  )
}
