
yaml_optenv_handler <- function(obj) {
  expr <- parse(text = obj)
  eval(expr)
}

yaml_withenv_handler <- function(yb) {
  yaml::yaml.load(
    yb,
    handlers = list(
      with_env = function(x) {
        e <- Sys.getenv() %>% new_environment()
        glue(x, .envir = e)
      }
    )
  )  
}
