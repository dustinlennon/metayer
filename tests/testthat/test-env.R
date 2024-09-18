test_that("top level", {
  test_sanitize()

  active_ns <- if (is_testing()) {
    getNamespace("metayer")
  } else {
    global_env()
  }

  expect_equal(
    wrap_get_namespace(current_env()),
    wrap_get_namespace(active_ns)
  )
})


test_that("env_rename", {
  test_sanitize()

  e <- new_environment() %>%
    env_rename("foo")

  expect_equal(
    attr(e, "name"),
    "foo"
  )
})

test_that("env_stack", {
  test_sanitize()

  e1 <- new_environment() %>%
    env_rename("one")

  e2 <- new_environment(parent = e1) %>%
    env_rename("two")

  e3 <- new_environment(parent = e2) %>%
    env_rename("three")

  full_stack_names <- env_stack(e3) %>%
    purrr::map_chr(
      \(e) attr(e, "name") %||% "R_EmptyEnv"
    ) %>% 
    unname()

  expect_equal(
    full_stack_names,
    c("three", "two", "one", "R_EmptyEnv")
  )

  trim_stack_names <- env_stack(e3, e1) %>%
    purrr::map_chr(
      \(e) attr(e, "name") %||% "R_EmptyEnv"
    ) %>% 
    unname()

  expect_equal(
    trim_stack_names,
    c("three", "two", "one")
  )
})
