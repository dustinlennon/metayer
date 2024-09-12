test_that("wrapped cli", {
  test_sanitize()

  ns <- wrap_get_namespace(current_env())
  collect <- function(code, .raise = FALSE) {
    with_wrapped_logger(
      code,
      namespace = ns,
      .raise = .raise
    )
  }

  alert <- wrapped_factory("cli::cli_alert", wrapper_cli, level = logger::INFO)
  inform <- wrapped_factory("cli::cli_inform", wrapper_cli, level = logger::INFO)
  warn <- wrapped_factory("cli::cli_warn", wrapper_cli, level = logger::WARN)
  abort <- wrapped_factory("cli::cli_abort", wrapper_cli, level = logger::ERROR)

  expect_equal(
    collect(alert("alert")),
    glue("{ns} INFO alert")
  )

  expect_equal(
    collect(inform("inform")),
    glue("{ns} INFO inform")
  )

  expect_equal(
    collect(warn("warn")),
    glue("{ns} WARN warn")
  )

  expect_equal(
    collect(abort("abort")),
    glue("{ns} ERROR abort")
  )

  expect_warning(
    collect(warn("warn"), .raise = TRUE),
    "warn"
  )

  expect_error(
    collect(abort("abort"), .raise = TRUE),
    "abort"
  )
})