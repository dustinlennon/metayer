
fin <- here::here("vignettes/examples.Rmd")
fout <- here::here("tmp/examples.out.md")

local({
  tmp_src <- tempfile()
  tmp_dest <- tempfile()

  # Rewrite knitr blocks to look like CodeBlocks
  xfun::read_utf8(fin) %>%
    purrr::map_chr(
      \(line) sub("^```{r}$", "``` {.r .knitr}", line, perl = TRUE)
    ) %>%
    xfun::write_utf8(tmp_src)

  rmarkdown::pandoc_convert(
    tmp_src,
    from = "markdown+yaml_metadata_block",
    to = "markdown",
    output = tmp_dest,
    options = c("--standalone")
  )

  # Restore knitr CodeBlocks to normal syntax
  xfun::read_utf8(tmp_dest) %>%
    purrr::map_chr(
      \(line) sub("^``` {.r .knitr}$", "```{r}", line, perl = TRUE)
    ) %>% 
    xfun::write_utf8(fout)
})