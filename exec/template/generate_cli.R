workflow()

cli_exports <- getNamespaceExports("cli")
cli_names <- grep("^cli_", cli_exports, value = TRUE) %>%
  sort()

ops <- list(
  info = list(),
  warn = list(
    "cli_alert_danger",
    "cli_alert_warning",
    "cli_warn"
  ),
  error = list(
    "cli_abort"
  )
)

get_level <- function(ops, name) {
  key <- names(which.max(sapply(ops, function(l) name %in% l)))
  switch(
    key,
    info = "logger::INFO",
    warn = "logger::WARN",
    error = "logger::ERROR"
  )
}

cli_levels <- cli_names %>%
  set_names() %>%
  purrr::map(
    \(n) get_level(ops, n)
  )

obj <- list(
  output = "./R/cli_wrapped.R",
  exports = cli_levels
)

yaml::write_yaml(obj, "./exec/template/wraps_cli.yml")
