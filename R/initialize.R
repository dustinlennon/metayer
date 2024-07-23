.onLoad <- function(libname, pkgname) {
  is_authoring <- isTRUE(getOption("knitr.in.progress")) ||
    isTRUE(getOption("jupyter.in_kernel"))

  options(
    metayer.verbosity = 30,
    metayer.transformer = null_aware_transformer,
    cli.default_handler = metayer_handler
  )  

  if (is_authoring) {
    options(
      cli.default_handler = null_handler
    )
    initialize_vignette()
  }
}
