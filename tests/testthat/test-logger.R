test_that("loggers return reasonable error classes", {

  test_sanitize()

  expect_error(
    log_abort("foo", .class = "logger-error"),
    class = "logger-error"
  )

})

test_that("log_alert_* should handle NULL values", {

  test_sanitize()

  foo <- "foo"
  bar <- NULL
  s <- "{foo} {bar}"

  # default cli_alert_info behavior
  expect_equal(
    withr::with_options(
      cli_test_opts(), 
      cli::cli_fmt(
        cli::cli_alert_info(s)
      )
    ),
    "i foo "
  )

  # cli_alert_info behavior with app_transformer and default_handler
  expect_equal(
    with_message_buf(
      withr::with_options(
        c(
          cli_test_opts(), 
          cli.default_handler = getOption("cli.default_handler") %||% default_handler,
          metayer.transformer = app_transformer
        ),
        cli::cli_alert_info(s)
      )
    ),
    "i foo NULL"
  )

  expect_equal(
    with_message_buf(
      withr::with_options(
        cli_test_opts(), 
        log_alert_info(s)
      )
    ),
    "i foo NULL"
  )

})


test_that("log_* should handle NULL values", {

  test_sanitize()

  foo <- "foo"
  bar <- NULL
  s <- "{foo} {bar}"

  # If we don't set metayer.verbosity, the default level excludes log_inform.
  expect_equal(
    with_message_buf(
      log_inform(s)
    ),
    ""
  )

  expect_equal(
    with_message_buf(
      withr::with_options(
        cli_test_opts(),
        log_inform(s)
      )
    ),
    "foo NULL"
  )

  expect_equal(
    catch_cnd(
      log_warn(s)
    ) %>%
      conditionMessage(),
    "foo NULL"
  )

  expect_equal(
    catch_cnd(
      log_abort(s)
    ) %>%
      conditionMessage(),
    "foo NULL"
  )

})