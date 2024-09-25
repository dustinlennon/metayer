test_that("pander context", {
  test_sanitize()

  logfile <- tempfile()

  get("namespaces", envir = asNamespace("logger"))$global %>%
    purrr::map(\(x) x$threshold)

  test_namespace <- get_namespace_name()

  expect_message({
    with_mocked_bindings(
      {
        with_pander({
          message("hello")
        })
      },
      log_level = mocked_log_level_factory(logfile)
    )
  })
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    glue("{test_namespace} INFO hello")
  )
})