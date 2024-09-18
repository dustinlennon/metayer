test_that("it works in test", {
  test_sanitize()

  wrapper <- function(cmd, args) {
    do.call(cmd, args)
  }

  myfn <- function(x, y) x + y

  wfn <- wrapped_factory("myfn", wrapper)
  expect_equal(wfn(3, 5), 8)
})

test_that("apply a simple wrapper", {
  test_sanitize()

  wrapper <- function(cmd, args, .name = "wrapper") {
    print(glue("called '{.name}'"))
    do.call(cmd, args)
  }

  # myfn will search in environment in which is was defined
  offset <- 1
  myfn <- function(x, y) offset + x + y
  args <- list(x = 3, y = 5)

  expect_output(
    wrapper(myfn, args),
    "called 'wrapper'"
  )

  expect_equal(
    withr::with_output_sink(
      "/dev/null",
      wrapper(myfn, args)
    ),
    9
  )

  wfn <- wrapped_factory("myfn", wrapper)

  expect_output(
    wfn(3, 5),
    "called 'myfn'"
  )

  expect_equal(
    withr::with_output_sink("/dev/null", wfn(3, 5)),
    9
  )
})


test_that("apply a more complicated wrapper", {
  test_sanitize()

  # extend wrapper-specific parameters to wrapped function
  wrapper <- function(cmd, args, offset = 0, .name = "wrapper") {
    local({
      environment(cmd) <- environment()
      do.call(cmd, args)
    })
  }

  offset <- 3
  myfn <- function(x, y) offset + x + y
  args <- list(x = 3, y = 5)

  wfn0 <- wrapped_factory("myfn", wrapper)
  expect_equal(
    wfn0(3, 5),
    8
  )

  wfn1 <- wrapped_factory("myfn", wrapper, offset = 1)
  expect_equal(
    wfn1(3, 5),
    9
  )

  wfn2 <- wrapped_factory("myfn", wrapper, offset = 2)
  expect_equal(
    wfn2(3, 5),
    10
  )
})
