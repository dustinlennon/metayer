test_that("config expr works", {
  test_sanitize()

  expect_equal(
    options()$uuid.generator.seed,
    12345L
  )

  expect_equal(
    mty_uuid(),
    "d5ca88e6-6c053-39b1e-e61fe-e3591b5ed9295"
  )

  expect_equal(
    mty_uuid(),
    "2e78c3d3-33f8a-a650e-e9dba-a89af87f575f7"
  )

  expect_equal(
    mty_uuid(12345L),
    "d5ca88e6-6c053-39b1e-e61fe-e3591b5ed9295"
  )
})
