test_that("mocked log level substitution", {
  test_sanitize()

  logfile <- tempfile()

  with_mocked_bindings(
    {
      foo <- 42
      log_level(logger::INFO, "foo {foo}", namespace = "superduper")
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    "superduper INFO foo 42"
  )
})

test_that("mocked_log_level", {
  test_sanitize()
  logfile <- tempfile()

  with_mocked_bindings(
    {
      foo <- 42
      log_info("foo {foo}", namespace = "superduper")
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    "superduper INFO foo 42"
  )

  with_mocked_bindings(
    {
      foo <- 42
      log_level(logger::INFO, "foo {foo} {NULL}", namespace = "superduper")
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z2 <- xfun::read_utf8(logfile)

  expect_equal(
    z2,
    "superduper INFO foo 42 <null>"
  )
})

test_that("conditions", {
  test_sanitize()

  logfile <- tempfile()
  expect_message({
    with_mocked_bindings(
      {
        foo <- 42
        with_logger({
          message("foo {foo}")
        })
      },
      log_level = mocked_log_level_factory(logfile)
    )
  })
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    "global INFO foo 42"
  )
})