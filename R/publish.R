#' convert a notebook to rmarkdown
#' 
#' Convert a jupyter notebook to a knitr-ready rmarkdown.
#' @param ipynb_in the ipynb filename
#' @param rmd_out the rmd filename
#' @export
pub_ipynb_to_rmd <- function(ipynb_in, rmd_out = NULL) {
  log_debug("pub_ipynb_to_rmd: input: {ipynb_in}")

  nb_conf <- ipynb_yaml_extract(ipynb_in)
  rdoc_conf <- config_get("rdoc_config")

  article_name <- nb_conf$title %||% "untitled"
  rdoc_conf$vignette <- whisker::whisker.render(
    rdoc_conf$vignette,
    list(article_name = article_name)
  )    

  conf <- update_list(rdoc_conf, nb_conf)

  rmd_out <- rmd_out %||% tempfile(fileext = ".Rmd")
  
  rmarkdown::convert_ipynb(
    input = ipynb_in,
    output = tempfile()
  ) %>%
    pub_rmd_to_rmd(rmd_out, conf = conf)

  log_debug("pub_ipynb_to_rmd: output: {rmd_out}")
  invisible(rmd_out)
}

#' rewrite an rmarkdown file
#' 
#' This is part of a multi-step process that converts jupyter notebooks into viable, knitr-ready
#' Rmd files.
#' 
#' In particular, it encodes knitr cell headers into a class before calling pandoc.  
#' This is required because pandoc misunderstands the Rmd syntax. The call to pandoc strips out
#' the existing YAML.  Then, the user supplied YAML is written out, followed by a decoding of the
#' class tag to restore any original knitr  cell headers.
#' 
#' @param rmd_in input file, an rmarkdown file as prepared by rmarkdown::convert_ipynb
#' @param rmd_out output file, an rmarkdown file
#' @param conf a nested list that will be converted into a YAML header
#' @export
pub_rmd_to_rmd <- function(
    rmd_in,
    rmd_out = NULL,
    conf = list()) {

  log_debug("pub_rmd_to_rmd: input: {rmd_in}")

  rmd_out <- rmd_out %||% tempfile(fileext = ".Rmd")

  dict <- new_environment()

  # map knitr cell headers into a class
  rmd_enc <- tempfile()
  xfun::read_utf8(rmd_in) %>%
    purrr::modify_if(
      is_knitr_chunk_start,
      \(l) encode_knitr(l, dict)
    ) %>%
    xfun::write_utf8(rmd_enc)
  log_debug("pub_rmd_to_rmd: created file: {rmd_enc}")

  # remove any yaml metadata added upstream (e.g., fixed title from convert_ipynb)
  rmd_raw <- tempfile()
  rmarkdown::pandoc_convert(
    rmd_enc,
    from = "markdown+yaml_metadata_block",
    to = "markdown",
    output = rmd_raw
  )
  log_debug("pub_rmd_to_rmd: created file: {rmd_raw}")

  # combine user supplied config and markdown in a single file with YAML header
  header <- sprintf("---\n%s---\n", yaml::as.yaml(conf))
  xfun::write_utf8(header, rmd_out)

  shim <- config_get("knitr", "shim")
  if (!is_null(shim)) {
    xfun::append_utf8(shim, rmd_out, sort = FALSE)
  }

  xfun::read_utf8(rmd_raw) %>% 
    purrr::modify_if(
      is_knitr_enc,
      \(l) decode_knitr(l, dict)
    ) %>%
    xfun::append_utf8(rmd_out, sort = FALSE)

  log_debug("pub_rmd_to_rmd: output: {rmd_out}")
  invisible(rmd_out)
}

#' knit an rmarkdown file
#' 
#' Knit an rmarkdown file into a markdown file suitable for pandoc processing.
#' 
#' @param rmd_in input file, an Rmd file as prepared by rmarkdown::convert_ipynb
#' @param md_out output file, an Rmd file
#' @param dev for knitr.chunk.dev
#' @export
pub_rmd_to_md <- function(rmd_in, md_out = NULL, dev = NULL) {
  md_out <- md_out %||% tempfile(fileext = ".md")

  withr::with_options(
    list(knitr.chunk.dev = dev),
    knitr::knit(rmd_in, output = md_out)  
  )
}
