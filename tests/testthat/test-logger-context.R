test_that("namespace context", {
  test_sanitize()

  logfile <- tempfile()

  test_namespace <- get_namespace_name()

  # withr::local_environment(env(), pos = ".GlobalEnv")

  # create a function, and put a copy in the metayer namespace
  foo <- function() log_info("24")
  ns <- new_environment(parent = getNamespace("base"))
  ns$foo <- set_env(foo, ns)

  with_mocked_bindings(
    {
      foo()
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    glue("{test_namespace} INFO 24")
  )

  with_mocked_bindings(
    {
      ns$foo()
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z2 <- xfun::read_utf8(logfile)

  expect_equal(
    z2,
    "base INFO 24"
  )

})


test_that("pubcontext", {
  test_sanitize()

  logfile <- tempfile()

  test_namespace <- get_namespace_name()

  # create a function, and put a copy in the metayer namespace
  foo <- function() log_info("24")
  ns <- new_environment(parent = getNamespace("base"))
  ns$foo <- set_env(foo, ns)

  with_mocked_bindings(
    {
      pubcontext(
        interactive_code = {
          foo()
        },
        non_interactive_code = {
          foo()
        },
        raise = TRUE
      )
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    glue("{test_namespace} INFO 24")
  )

  with_mocked_bindings(
    {
      pubcontext(
        interactive_code = {
          ns$foo()
        },
        non_interactive_code = {
          ns$foo()
        },
      )
    },
    log_level = mocked_log_level_factory(logfile)
  )
  z1 <- xfun::read_utf8(logfile)

  expect_equal(
    z1,
    "base INFO 24"
  )


})