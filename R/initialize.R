.onLoad <- function(libname, pkgname) {
  is_authoring <- isTRUE(getOption("knitr.in.progress")) ||
    isTRUE(getOption("jupyter.in_kernel"))

  options(
    cli.default_handler = metayer_cli_handler,
    metayer.verbosity = 20,
    metayer.transformer = null_aware_transformer
  )  

  if (is_authoring) {
    initialize_vignette()
  }

  # create the metayer version of a cliapp object
  metayer_app_factory(
    app = cli::default_app()
  )
}
