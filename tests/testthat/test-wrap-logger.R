test_that("wrapped cli", {
  test_sanitize()

  expect_equal(
    with_wrapped_logger(
      {
        log_info("wwl", namespace = "foo")
      },
      namespace = "foo"
    ),
    "foo INFO wwl"
  )
})
