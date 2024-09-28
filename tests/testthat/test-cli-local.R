test_that("test one", {
  test_sanitize()

  test_namespace <- get_namespace_name()

  logfile <- tempfile()
  expect_message({
    with_mocked_bindings(
      {
        local({
          foo <- "42"
          cli_verbatim(foo, "bar")
        })
      },
      log_level = mocked_log_level_factory(logfile)
    )
  })
  z1 <- xfun::read_utf8(logfile)

  on.exit(fs::file_delete(logfile))
  expect_equal(
    z1,
    c(
      glue("{test_namespace} INFO 42"),
      "bar"
    )
  )
})
