testthat("function rebuild", {

  test_sanitize()

  options(
    cli.default_handler = logged_cli_handler
  )

  logger::log_threshold(TRACE)
  logger::log_formatter(formatter_paste)
  logger::log_appender(appender_stdout)  

  pkg <- "cli"
  name <- "cli_ol"
  level <- logger::INFO

  g <- wrap_factory_safe(pkg, name, level)
  g

  g(c(1, 2, 3))


  h <- wrap_factory_safe("cli", "cli_alert_warning", logger::WARN)
  h("peacock")
})


testthat("dynamic wrappers", {

  test_sanitize()

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
    cli = list(
      output = "./R/cli_wrapped.R",
      exports = cli_levels
    )
  )

  yaml::write_yaml(obj, "./exec/template/wraps_cli.yaml")
})