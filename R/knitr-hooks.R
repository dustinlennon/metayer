#' The metayer/knitr hook used for preprocessing / postprocessing
#' 
#' Refer to [knitr documentation](https://yihui.org/knitr/hooks).  This gives us
#' an opportunity to rewrite a chunk's output at the knit.md stage.  In particular,
#' we use this for images:
#' * HTML images are encoded as base64 and wrapped in an img tag
#' * PDF images are referenced with a markdown construction
#' 
#' @keywords internal
#' @param before a boolean
#' @param options the current chunk options
#' @param envir the environment in which our code will be executed
#' @param name name associated with the hook, e.g. "metayer"
#' @param ... to match knitr hook signature
#' @export
knitr_metayer_hook <- function(before, options, envir, name, ...) {
  if (before == FALSE) {
    chunk_env <- storage_env("metayer", "chunks", options$label)
    if (!is.null(chunk_env$output)) {
      options$results <- FALSE
      return(chunk_env$output)
    }
  }
  invisible(NULL)
}
