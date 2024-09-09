test_that("wrapped cli", {
  test_sanitize()

  mock <- function(expr, .raise = FALSE, .envir = parent.frame()) {
    tmp <- withr::local_tempfile()

    with_mocked_bindings(
      {
        tryCatch(
          eval(expr, envir = .envir),
          error = function(cnd) {
            if (.raise) cnd_signal(cnd)
          },
          warning = function(cnd) {
            if (.raise) cnd_signal(cnd)
          }
        )
      },
      log_level = log_level_mock_factory(tmp),
      .package = "logger"
    )

    xfun::read_utf8(tmp)
  }

  alert <- wrapped_factory("cli::cli_alert", wrapper_cli, level = logger::INFO)
  inform <- wrapped_factory("cli::cli_inform", wrapper_cli, level = logger::INFO)
  warn <- wrapped_factory("cli::cli_warn", wrapper_cli, level = logger::WARN)
  abort <- wrapped_factory("cli::cli_abort", wrapper_cli, level = logger::ERROR)

  expect_equal(
    mock(alert("alert")),
    ">>> global.cli INFO alert"
  )

  expect_equal(
    mock(inform("inform")),
    ">>> global.cli INFO inform"
  )

  expect_equal(
    mock(warn("warn")),
    ">>> global.cli WARN warn"
  )

  expect_equal(
    mock(abort("abort")),
    ">>> global.cli ERROR abort"
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