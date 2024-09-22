test_that("wrapped cli with mocked logger", {
  test_sanitize()

  logfile <- tempfile()

  alert <- wrapped_factory("cli::cli_alert", wrapped_with_logger, level = logger::INFO)
  with_mocked_bindings(
    {
      alert("foo") %>%
        suppressMessages()
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    "global INFO > foo"
  )

  warn <- wrapped_factory("cli::cli_alert_warning", wrapped_with_logger, level = logger::WARN)
  with_mocked_bindings(
    {
      warn("foo") %>%
        suppressMessages()
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z2 <- xfun::read_utf8(logfile)

  expect_equal(
    z2,
    "global WARN ! foo"
  )
})

test_that("mty.cli_null in cli methods", {
  test_sanitize()

  logfile <- tempfile()

  alert <- wrapped_factory("cli::cli_alert", wrapped_with_logger, level = logger::INFO)
  with_mocked_bindings(
    {
      foo <- 42
      alert("foo {foo} {NULL}") %>%
        suppressMessages()
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    "global INFO > foo 42 <null>"
  )

})


test_that("cli rlang wrappers", {
  test_sanitize()

  logfile <- tempfile()

  expect_warning(
    with_mocked_bindings(
      {
        foo <- 42
        cli_warn("foo {foo} {NULL}")
      },
      log_level = mocked_log_level_factory(logfile)
    )
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    "global WARN foo 42"   # no "<null>" b/c no codepath through cli_nullity_handler
  )

  # This should still cause an error, but it should generate a log record before propagating
  # the exception.
  expect_error({
    with_mocked_bindings(
      {
        foo <- 42
        cli_abort("foo {foo}")
      },
      log_level = mocked_log_level_factory(logfile)
    )
  })

  z2 <- xfun::read_utf8(logfile)
  expect_equal(
    z2,
    "global ERROR foo 42"
  )
})


test_that("cli rlang wrappers with dots", {
  test_sanitize()

  # keep testthat output clean: send logger output to a tempfile
  local_mocked_bindings(
    log_level = mocked_log_level_factory(tempfile())
  )

  w <- catch_cnd({
    cli_warn("foo", bar = 42)
  })

  expect_equal(
    w$bar,
    42
  )
})