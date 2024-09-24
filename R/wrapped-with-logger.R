#' Wrap a function for logging
#' 
#' @param cmd a cli function, e.g., cli::cli_alert
#' @param args the args to be passed to the cmd via do.call
#' @param level the logger level to be associated with the cli method
#' @export
wrapped_with_logger <- function(
    cmd,
    args,
    level = NULL) {

  # # debug via RDA file
  # f <- "/home/dnlennon/Workspace/repos/metayer/tmp/wwl.rda"  
  # if (fs::file_exists(f)) {    
  #   fs::file_delete(f)
  # }

  # # save environments
  # current <- current_env()
  # caller <- caller_env()
  # parent <- parent.frame()

  # args will be subbed in by wrapped_factory
  cc_args <- args

  # handle dots args
  mc_args <- call_match(defaults = TRUE, dots_expand = FALSE) %>%
    call_args()
  dots <- mc_args$...
  
  # create a placeholder, if necessary
  if (!is_null(dots)) {
    cc_args$... <- NA
  }

  # flatten dots, if any
  z_keys <- list()
  z_vals <- list()
  off <- 1
  for (i in seq_along(cc_args)) {
    k <- names(cc_args)[i]

    if (k == "...") {
      for (i2 in seq_along(dots)) {
        z_keys[[off]] <- names(dots)[i2] %||% ""
        z_vals[[off]] <- dots[[i2]]
        off <- off + 1
      }
    } else {
      z_keys[[off]] <- k
      z_vals[[off]] <- get0(k, current_env())
      off <- off + 1
    }
  }
  names(z_vals) <- z_keys

  # save(current, caller, parent, z_vals, cc_args, file = f)

  # construct the call
  client_call <- c(
    substitute(cmd),
    z_vals
  ) %>% as.call()

  # save(current, caller, parent, client_call, z_vals, mc_args, file = f)

  with_logger(
    client_call,
    .local_envir = parent.frame(),
    level = level
  )
}
