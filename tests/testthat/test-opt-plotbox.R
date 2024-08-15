test_that("distortion calculation", {

  test_sanitize()

  dx <- 1
  dy <- 10
  xopt <- opt_plotbox(7, 7, dx, dy, distortion_ratio = 2)
  drat <- (xopt$w / xopt$h) / (dx / dy)

  expect_equal(xopt$w, 1.4)
  expect_equal(xopt$h, 7.0)
  expect_equal(xopt$drat, drat)

  xopt <- opt_plotbox(7, 7, dy, dx, distortion_ratio = 2)
  drat <- (xopt$w / xopt$h) / (dy / dx)

  expect_equal(xopt$w, 7.0)
  expect_equal(xopt$h, 1.4)
  expect_equal(xopt$drat, drat)
})