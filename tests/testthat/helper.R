#' An option set for cli text output
cli_test_opts <- function(metayer_transformer = null_aware_transformer) {
  num_colors <- 1
  list(
    cli.dynamic = FALSE,
    cli.ansi = FALSE,
    cli.unicode = FALSE,
    crayon.enabled = num_colors > 1,
    crayon.colors = num_colors,
    metayer.verbosity = 0,
    metayer.transformer = metayer_transformer
  )
}
