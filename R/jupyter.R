#' An appender for jupyter (with stderr)
#' 
#' In jupyter, stderr will show up on the console, and messages will show up
#' in the notebook.  In the jupyter context, we also want to display something
#' that looks like the usual message output.
#' 
#' @keywords internal
appender_jupyter <- function(lines) {
  cat(lines, file = stderr(), sep = "\n")
  if (isTRUE(getOption("jupyter.in_kernel"))) {
    data <- list(
      `application/vnd.jupyter.stderr` = lines
    )
    IRdisplay::publish_mimebundle(data, NULL)
  } else if (isTRUE(getOption("knitr.in.progress"))) {
    lines %>%
      paste0(collapse = "\n") %>%
      cat("\n")
  }
}
