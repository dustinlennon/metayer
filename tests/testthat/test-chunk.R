test_that("knitr encode / decode", {
  test_sanitize()

  l <- "```{r, my-chunk, echo=FALSE, fig.height=4, dev='jpeg'}"

  expect_true(
    is_knitr_chunk_start(l)
  )

  dict <- new_environment()
  key <- "577eb481367bcbbb14e627cc2095da20"
  enc <- encode_knitr(l, dict, key = key)

  expect_equal(
    enc,
    "``` {.r .knitr-577eb481367bcbbb14e627cc2095da20}"
  )

  expect_true(
    is_knitr_enc(enc)
  )

  expect_equal(
    decode_knitr(enc, dict),
    "```{r, my-chunk, echo=FALSE, fig.height=4, dev='jpeg'}"
  )
})
