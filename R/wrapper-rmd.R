#' Wrap an rmarkdown document
#' 
#' This is intended to be used alongside with_monkey_patch
#' 
#' @keywords internal
#' @param cmd unused
#' @param args args to be passed to cmd (or func)
#' @param func the original function
wrapped_doc <- function(cmd, args, func) {
  fqn <- env_get(current_env(), ".name", inherit = TRUE)
  log_debug("{fqn} [monkey patched]")
  doc <- do.call(func, args)
  doc$knitr$knit_hooks$metayer <- hook_metayer
  doc
}
