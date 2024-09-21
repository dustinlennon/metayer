#' Wrapper for a cli method
#' 
#' This includes consideration for cli_inform, cli_warn, and cli_abort which 
#' use an rlang codepath.
#' 
#' @keywords internal
#' @param cmd a cli function, e.g., cli::cli_alert
#' @param args the args to be passed to the cmd via do.call
#' @param level the logger level to be associated with the cli method
wrapper_cli <- function(
    cmd,
    args,
    level) {

  with_logger(
    {
      do.call(cmd, args)
    },
    level = level
  )
}
