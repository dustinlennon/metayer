# testthat("function rebuild", {

#   test_sanitize()

#   options(
#     cli.default_handler = logged_cli_handler
#     # cli.default_handler = metayer_cli_handler
#     # cli.default_handler = NULL
#   )

#   # reset logger namespaces
#   log_reset <- function() {
#     ns <- grep("global", log_namespaces(), value = TRUE, invert = TRUE)
#     for (key in ns) {
#       env_unbind(logger:::namespaces, key)
#     }
#   }

#   log_threshold(INFO)
#   log_appender(appender_stdout)  

#   use_json_logging <- FALSE
  
#   if (use_json_logging) {
#     log_formatter(formatter_json)
#     log_layout(
#       layout_json(fields = c("time", "ns", "level", "msg"))
#     )
#   } else {
#     log_formatter(formatter_glue)
#     log_layout(
#       layout_glue_generator(
#         format = "{time} {ns} {level} : {msg}"
#       )
#     )
#   }

#   log_info("hello world {42}")

#   log_reset()
#   log_threshold(TRACE, namespace = "metayer")
#   log_formatter(formatter_paste, namespace = "global.cli")
#   log_formatter(formatter_paste, namespace = "metayer.cli")

#   # wrapping w/ level
#   pkg <- "cli"
#   name <- "cli_ol"
#   level <- logger::WARN

#   g <- wrap_factory_safe(pkg, name, level)
#   g(c(1, 2, 3))

#   # NULL testing
#   foo <- 42
#   bar <- "zzz"
#   cli_alert_info("foo {.emph {foo}} {foo} {bar} {NULL}")

#   # abort
#   cli_abort("stopping now!")

#   cli_warn("a stern warning")

#   # check namespace (global.cli)
#   names <- env_stack(caller_env()) %>%
#     purrr::map_vec(env_name)
#   cli_ol(names)

#   # metayer.cli
#   pkg_demo_cli()

# })

test_that("injection get command", {
  test_sanitize()

  pkg <- "cli"
  name <- "cli_text"

  cmd <- inj_get_cmd(pkg, name)
  
  expect_equal(
    deparse(cmd, width.cutoff = 200L),
    "cli::cli_text(..., .envir = .envir)"
  )
})


test_that("inj_body_cli wraps metadata", {
  log_threshold(INFO)
  logger_reset()
  test_sanitize()
  logger_test_init()

  log_threshold(OFF, namespace = "metayer")

  # construct a cmd and corresponding function
  foo <- 42
  qn <- str2lang("cli::cli_text")
  cmd <- as.call(c(qn, alist("foo = {foo}", .envir = .caller_env)))

  fn <- bind_cmd(inj_body_cli, cmd, level = logger::INFO)

  # check calling context
  out1 <- with_message_buf(
    with_mocked_bindings(
      inj_get_namespace = function(...) "global.cli",
      fn(.caller_env = current_env())
    )
  )

  expect_equal(
    out1,
    "global.cli INFO : foo = 42"
  )

})


test_that("basic wrap_factory call", {
  logger_reset()
  test_sanitize()
  logger_test_init()

  log_threshold(OFF, namespace = "metayer")

  fn <- wrap_factory("cli", "cli_text", logger::INFO)

  out1 <- with_message_buf(
    with_mocked_bindings(
      inj_get_namespace = function(...) "metayer.cli",
      {
        foo <- 42
        fn("foo = {foo}")
      }
    )
  )

  expect_equal(
    out1,
    "metayer.cli INFO : foo = 42"
  )
  
})


test_that("wrapped cli_text", {
  logger_reset()
  test_sanitize()
  logger_test_init()

  log_threshold(OFF, namespace = "metayer")

  out1 <- with_message_buf(
    with_mocked_bindings(
      inj_get_namespace = function(...) "metayer.cli",
      {
        foo <- 42
        cli_text("foo = {foo}")
      }
    )
  )

  expect_equal(
    out1,
    "metayer.cli INFO : foo = 42"
  )

})

test_that("wrapped cli_abort", {
  logger_reset()
  test_sanitize()
  logger_test_init()

  log_threshold(OFF)

  expect_error(
    cli_abort("abort", class = "test-case"),
    class = "test-case"
  )

  cnd <- catch_cnd(
    cli_abort("abort", class = "test-case")
  )

  expect_equal(
    format(cnd, backtrace = FALSE),
    "<error/test-case>\nError:\n! abort"
  )

  expect_contains(
    class(cnd),
    "rlang_error"
  )

})

test_that("call inj_body_cli directly", {
  log_threshold(INFO)
  logger_reset()
  test_sanitize()
  logger_test_init()

  log_threshold(OFF, namespace = "metayer")

  # construct a cmd and corresponding function
  foo <- 42
  qn <- str2lang("cli::cli_text")
  cmd <- as.call(c(qn, alist("foo = {foo}", .envir = .caller_env)))

  out1 <- with_message_buf(
    with_mocked_bindings(
      inj_get_namespace = function(...) "metayer.cli",
      {
        inj_body_cli(
          cmd,
          level = logger::WARN
        )
      }
    )
  )

  expect_equal(
    out1,
    "metayer.cli WARN : foo = 42"
  )
})