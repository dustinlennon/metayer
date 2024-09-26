test_that("create, use, purge storage in default global container", {

  test_sanitize()

  foo <- storage_env("foo")
  expect_in("foo", names(.storage))
  expect_type(.storage, "environment")
  expect_equal(attr(foo, "name"), ".storage.foo")

  bar <- storage_env("foo", "bar")
  expect_in("bar", names(.storage$foo))

  storage_purge()

  expect_error(
    .storage,
    regexp = "'.storage' not found"
  )

})

test_that("create, use, purge storage in specified container", {

  test_sanitize()

  container <- new_environment()

  foo <- storage_env("foo", .envir = container)
  expect_in("foo", names(container$.storage))
  expect_type(container$.storage, "environment")
  expect_equal(attr(foo, "name"), ".storage.foo")

  bar <- storage_env("foo", "bar", .envir = container)
  expect_in("bar", names(container$.storage$foo))

  storage_purge(.envir = container)

  expect_null(
    container$.storage
  )
})
