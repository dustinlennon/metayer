
yaml_handler_optenv <- function(ys) {
  expr <- parse(text = ys)
  eval(expr)
}

yaml_handler_with_env <- function(ys) {
  e <- Sys.getenv() %>% new_environment()
  glue(ys, .envir = e)
}

yaml_handler_keep_optenv <- function(ys) {
  glue("!optenv {ys}")
}

yaml_handler_keep_with_env <- function(ys) {
  glue("!with_env {ys}")
}
