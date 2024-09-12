test_that("wrapped cli", {
  test_sanitize()

  ns <- wrap_get_namespace(current_env())

  mock <- function(expr, .raise = FALSE, .envir = parent.frame()) {    
    with_wrapped_logger(
      {
        tryCatch({
          force(expr)
        },
        error = function(cnd) {
          if (.raise) cnd_signal(cnd)
        },
        warning = function(cnd) {
          if (.raise) cnd_signal(cnd)
        })
      },
      namespace = ns
    )
  }

  alert <- wrapped_factory("cli::cli_alert", wrapper_cli, level = logger::INFO)
  inform <- wrapped_factory("cli::cli_inform", wrapper_cli, level = logger::INFO)
  warn <- wrapped_factory("cli::cli_warn", wrapper_cli, level = logger::WARN)
  abort <- wrapped_factory("cli::cli_abort", wrapper_cli, level = logger::ERROR)

  expect_equal(
    mock(alert("alert")),
    glue("{ns} INFO alert")
  )

  expect_equal(
    mock(inform("inform")),
    glue("{ns} INFO inform")
  )

  expect_equal(
    mock(warn("warn")),
    glue("{ns} WARN warn")
  )

  expect_equal(
    mock(abort("abort")),
    glue("{ns} ERROR abort")
  )

  expect_warning(
    mock(warn("warn"), .raise = TRUE),
    "warn"
  )

  expect_error(
    mock(abort("abort"), .raise = TRUE),
    "abort"
  )
})