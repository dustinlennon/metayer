testthat("knitr encode / decode", {
  test_sanitize()

  l <- "```{r, my-chunk, echo=FALSE, fig.height=4, dev='jpeg'}"

  expect_true(
    is_knitr(l)
  )

  dict <- new_environment()
  enc <- encode_knitr(l, dict)
  key <- "dbdda0b47f8821b90662b6d4dea8ee49"

  expect_equal(
    enc,
    "``` {.r .knitr-dbdda0b47f8821b90662b6d4dea8ee49}"
  )

  expect_equal(
    dict[[key]],
    "r, my-chunk, echo=FALSE, fig.height=4, dev='jpeg'"
  )

  expect_true(
    is_knitr_enc(enc)
  )

  expect_equal(
    decode_knitr(enc, dict),
    l
  )
})


testthat("postprocessing", {

  test_sanitize()

  ipynb <- test_path("fixtures/vignettes/examples.ipynb")
  rmarkdown::convert_ipynb(ipynb)

  rmd <- test_path("fixtures/vignettes/examples.Rmd")

  tmp <- get0("tmp", ifnotfound = withr::local_tempfile())
  postprocess_rmd(rmd, tmp)
  lines <- xfun::read_utf8(tmp)

  expect_equal(
    lines[3],
    "title: \"metayer: examples\""
  )

  expect_equal(
    lines[6],
    "```{r}"
  )

  expect_equal(
    lines[25],
    "```{r, my-chunk, echo=FALSE, fig.height=4, dev='jpeg'}"
  )
})
