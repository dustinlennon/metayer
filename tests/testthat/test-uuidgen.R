test_that("config expr works", {
  test_sanitize()

  expect_equal(
    options()$uuid.salt,
    12345L
  )

  expect_equal(
    mty_uuid(),
    "d5ca88e6-d5ca-d5ca-d5ca-d5ca88e6c053"
  )

  expect_equal(
    mty_uuid(),
    "2e78c3d3-2e78-2e78-2e78-2e78c3d33f8a"
  )

  expect_equal(
    mty_uuid(12345L),
    "d5ca88e6-d5ca-d5ca-d5ca-d5ca88e6c053"
  )
})
