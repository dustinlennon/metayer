testthat("function rebuild", {

  test_sanitize()

  options(
    cli.default_handler = logged_cli_handler
    # cli.default_handler = metayer_cli_handler
    # cli.default_handler = NULL
  )

  # reset logger namespaces
  log_reset <- function() {
    ns <- grep("global", log_namespaces(), value = TRUE, invert = TRUE)
    for (key in ns) {
      env_unbind(logger:::namespaces, key)
    }
  }

  log_threshold(INFO)
  log_appender(appender_stdout)  

  use_json_logging <- FALSE
  
  if (use_json_logging) {
    log_formatter(formatter_json)
    log_layout(
      layout_json(fields = c("time", "ns", "level", "msg"))
    )
  } else {
    log_formatter(formatter_glue)
    log_layout(
      layout_glue_generator(
        format = "{time} {ns} {level} : {msg}"
      )
    )
  }

  log_info("hello world {42}")

  log_reset()
  log_threshold(TRACE, namespace = "metayer")
  log_formatter(formatter_paste, namespace = "global.cli")
  log_formatter(formatter_paste, namespace = "metayer.cli")

  # wrapping w/ level
  pkg <- "cli"
  name <- "cli_ol"
  level <- logger::WARN

  g <- wrap_factory_safe(pkg, name, level)
  g(c(1, 2, 3))

  # NULL testing
  foo <- 42
  bar <- "zzz"
  cli_alert_info("foo {.emph {foo}} {foo} {bar} {NULL}")

  # abort
  cli_abort("stopping now!")

  cli_warn("a stern warning")

  # check namespace (global.cli)
  names <- env_stack(caller_env()) %>%
    purrr::map_vec(env_name)
  cli_ol(names)

  # metayer.cli
  pkg_demo_cli()

})

