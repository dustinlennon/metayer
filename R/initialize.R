.onLoad <- function(libname, pkgname) {
  is_authoring <- isTRUE(getOption("knitr.in.progress")) ||
    isTRUE(getOption("jupyter.in_kernel"))

  options(
    metayer.cli.null = "<NULL>"
  )

  if (is_authoring) {
    initialize_vignette()
  }
}
