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
      cli_test_opts(metayer_transformer = NULL), 
      cli::cli_fmt(
        cli::cli_alert_info(s)
      )
    ),
    "i foo "
  )

  # cli_alert_info behavior with null_aware_transformer and metayer_handler
  expect_equal(
    with_message_buf(
      withr::with_options(
        c(
          cli_test_opts(), 
          cli.default_handler = metayer_handler,
          metayer.transformer = null_aware_transformer
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
    ) %>% (function(msg) {
      withr::with_options(
        cli_test_opts(),
        conditionMessage(msg)
      )    
    }),
    "foo NULL"
  )

  expect_equal(
    catch_cnd(
      log_abort(s)
    ) %>% (function(msg) {
      withr::with_options(
        cli_test_opts(),
        conditionMessage(msg)
      )    
    }),
    "foo NULL"
  )
  

})


test_that("can recover original behavors", {
  test_sanitize()

  foo <- 42
  bar <- NULL

  # no output
  expect_equal(
    with_message_buf(
      withr::with_options(
        list(),
        log_inform("hi {foo} {bar}")
      )
    ),
    ""
  )
    

  # default output
  expect_equal(
    with_message_buf(
      withr::with_options(
        list(
          metayer.verbosity = 0,
          metayer.transformer = NULL
        ),
        log_inform("hi {foo} {bar}")
      )
    ),
    "hi 42"
  )

  # NULL-aware output
  expect_equal(
    with_message_buf(
      withr::with_options(
        list(
          metayer.verbosity = 0,
          metayer.transformer = null_aware_transformer
        ),
        log_inform("hi {foo} {bar}")
      )
    ),
    "hi 42 NULL"
  )

  # default output
  expect_equal(
    with_message_buf(
      withr::with_options(
        cli_test_opts(metayer_transformer = NULL),
        log_alert_info("hi {foo} {bar}")
      )
    )
    , 
    "i hi 42 "
  )

  # NULL-aware output
  expect_equal(
    with_message_buf(
      withr::with_options(
        cli_test_opts(),
        log_alert_info("hi {foo} {bar}")
      )
    ),
    "i hi 42 NULL"
  )
})