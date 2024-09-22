#' Wrap a function for logging
#' 
#' @param cmd a cli function, e.g., cli::cli_alert
#' @param args the args to be passed to the cmd via do.call
#' @param level the logger level to be associated with the cli method
#' @export
wrapped_with_logger <- function(
    cmd,
    args,
    level) {

  # handle dots (call_match) and symbols (remap_symb) separately
  mc_args <<- call_match(defaults = TRUE, dots_expand = FALSE)
  dots <- as.list(mc_args$...)

  cc_args <- args
  cc_args <- cc_args %>%
    magrittr::inset2("...", NULL) %>%
    remap_symb()

  # construct the call
  client_call <- c(
    substitute(cmd),
    c(cc_args, dots)
  ) %>% as.call()

  with_logger(
    client_call,
    level = level
  )
}
