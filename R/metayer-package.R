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
  .metayer()
}
