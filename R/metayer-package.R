#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  .metayer()
}
