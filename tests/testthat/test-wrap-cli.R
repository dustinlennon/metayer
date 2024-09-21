test_that("wrapped cli with mocked logger", {
  test_sanitize()

  logfile <- tempfile()

  alert <- wrapped_factory("cli::cli_alert", wrapper_cli, level = logger::INFO)
  with_mocked_bindings(
    {
      alert("foo")
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    "namespace:metayer INFO > foo"
  )

  warn <- wrapped_factory("cli::cli_alert_warning", wrapper_cli, level = logger::WARN)
  with_mocked_bindings(
    {
      warn("foo")
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z2 <- xfun::read_utf8(logfile)

  expect_equal(
    z2,
    "namespace:metayer WARN ! foo"
  )
})

test_that("mty.cli_null in cli methods", {
  test_sanitize()

  logfile <- tempfile()

  alert <- wrapped_factory("cli::cli_alert", wrapper_cli, level = logger::INFO)

  with_mocked_bindings(
    {
      foo <- 42
      alert("foo {foo} {NULL}")
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    "namespace:metayer INFO > foo 42 <null>"
  )

})
