test_that("publishing context is working", {
  test_sanitize()

  ipynb <- test_path("fixtures/vignettes/examples.ipynb")

  # Get YAML config
  conf <- ipynb_yaml_extract(ipynb)

  rmd_tmp <- tempfile()
  rmd_out <- tempfile()
  rmarkdown::convert_ipynb(input = ipynb, output = rmd_tmp)

  process_rmd(rmd_tmp, rmd_out, conf)

  result <- xfun::read_utf8(rmd_out) %>%
    paste0(collapse = "\n")

  expect_snapshot(cat(result))

})


test_that("preprocess raw yaml cells", {
  test_sanitize()

  ipynb <- test_path("fixtures/vignettes/raw_yaml_test.ipynb")
  header <- ipynb_yaml_extract(ipynb)

  json <- jsonlite::toJSON(header, pretty = TRUE)

  expect_snapshot(cat(json))

})
