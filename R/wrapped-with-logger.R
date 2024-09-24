#' a with_logger wrapper 
#' 
#' This is suitable to be used with wrapped_factory to imbue another function with
#' logger functionality.
#' @param cmd a cli function, e.g., cli::cli_alert
#' @param args the args to be passed to the cmd via do.call
#' @param level the logger level to be associated with the cli method
#' @export
wrapped_with_logger <- function(
    cmd,
    args,
    level = NULL) {

  # args will be replaced with a symbolic list by wrapped_factory
  cc_args <- args

  # use call_match to handle dots
  mc_args <- call_match(defaults = TRUE, dots_expand = FALSE) %>%
    call_args()
  dots <- mc_args$...
  
  # create a placeholder, if necessary
  if (!is_null(dots)) {
    cc_args$... <- NA
  }

  # recover the values from symbols and flatten dots, if any
  z_keys <- list()
  z_vals <- list()
  j <- 1
  for (i in seq_along(cc_args)) {
    k <- names(cc_args)[i]

    if (k == "...") {
      for (i2 in seq_along(dots)) {
        z_keys[[j]] <- names(dots)[i2] %||% ""
        z_vals[[j]] <- dots[[i2]]
        j  <- j + 1
      }
    } else {
      z_keys[[j]] <- k
      z_vals[[j]] <- get0(k, current_env())
      j <- j + 1
    }
  }
  names(z_vals) <- z_keys

  # construct the call
  client_call <- c(
    substitute(cmd),
    z_vals
  ) %>% as.call()

  with_logger(
    client_call,
    .local_envir = parent.frame(),
    level = level
  )
}
