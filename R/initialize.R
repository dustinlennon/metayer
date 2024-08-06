.onLoad <- function(libname, pkgname) {
  is_authoring <- isTRUE(getOption("knitr.in.progress")) ||
    isTRUE(getOption("jupyter.in_kernel"))

  # Use R_CONFIG_ACTIVE to set config
  cfg <- config::get("metayer_options")

  options(
    cli.default_handler = bang_expr(
      cfg$cli.default_handler
    ),
    metayer.cli_null = cfg$metayer.cli_null,
    metayer.hash_label_length = cfg$metayer.hash_label_length
  )

  if (is_authoring) {
    initialize_vignette()
  }
}
