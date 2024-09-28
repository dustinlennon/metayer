#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
#' @importFrom grDevices dev.flush
#' @importFrom grDevices dev.off
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
#' @importFrom utils capture.output
#' @importFrom utils str
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  # Note: to pass R CMD check, set a vignette engine
  loadNamespace("knitr") 	
  engine <- tools::vignetteEngine("knitr::rmarkdown")
  do.call(tools::vignetteEngine, engine)

  .metayer()
}
