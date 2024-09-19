#' Convert ipynb to knitr-read rmd
#' 
#' @param ipynb_in the ipynb filename
#' @param rmd_out the rmd filename
#' @export
pub_ipynb_to_rmd <- function(ipynb_in, rmd_out = NULL) {
  log_debug("pub_ipynb_to_rmd: input: {ipynb_in}")

  conf <- ipynb_yaml_extract(ipynb_in)

  rmd_out <- rmd_out %||% tempfile(fileext = ".Rmd")
  
  rmarkdown::convert_ipynb(
    input = ipynb_in,
    output = tempfile()
    ) %>%
      pub_rmd_to_rmd(rmd_out, conf = conf)

  log_debug("pub_ipynb_to_rmd: output: {rmd_out}")
  invisible(rmd_out)
}

#' postprocess output of rmarkdown::convert_ipynb
#' 
#' This is part of a multi-step process that converts jupyter notebooks into viable knitr
#' Rmd files.  
#' 
#' In particular, it encodes knitr cell headers into a class before calling pandoc.  This is 
#' required because pandoc misunderstands the Rmd syntax.
#' 
#' The call to pandoc strips the existing YAML from the input.  Then, the user supplied YAML
#' is written out, followed by a decoding of the class tag to restore the original knitr 
#' cell headers.
#' 
#' @keywords internal
#' @param input input file, an Rmd file as prepared by rmarkdown::convert_ipynb
#' @param output output file, an Rmd file
#' @param conf a nested list that will be converted into a YAML header
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
  xfun::read_utf8(rmd_raw) %>% 
    purrr::modify_if(
      is_knitr_enc,
      \(l) decode_knitr(l, dict)
    ) %>%
    xfun::append_utf8(rmd_out, sort = FALSE)

  log_debug("pub_rmd_to_rmd: output: {rmd_out}")
  invisible(rmd_out)
}

#' Convert an rmd to a pdf
#' 
#' @param rmd_in the rmd filename
#' @param pdf_out the pdf filename
#' @export
pub_rmd_to_pdf <- function(rmd_in, pdf_out = NULL) {
  log_debug("pub_rmd_to_pdf: input: {rmd_in}")

  pdf_out <- pdf_out %||% tempfile(fileext = ".pdf")

  rmd_knit <- preknit(
    rmd_in,
    tempfile(fileext = ".Rmd"),
    "pdf"
  )
  log_debug("pub_rmd_to_pdf: created file: {rmd_knit}")

  xfun::Rscript_call(
    rmarkdown::render,
    list(
      input = rmd_knit,
      output_file = pdf_out,
      output_format = rmarkdown::pdf_document(),
      clean = FALSE 
    )
  )  

  log_debug("pub_rmd_to_pdf: output: {pdf_out}")
  invisible(pdf_out)
}

#' Convert an rmd to an html
#' 
#' @param rmd_in the rmd filename
#' @param html_out the pdf filename
#' @export
pub_rmd_to_html <- function(rmd_in, html_out = NULL) {
  log_debug("pub_rmd_to_html: input: {rmd_in}")

  html_out <- html_out %||% tempfile(fileext = ".html")

  rmd_knit <- preknit(
    rmd_in,
    tempfile(fileext = ".html"),
    "html"
  )
  log_debug("created file: {rmd_knit}")

  xfun::Rscript_call(
    rmarkdown::render,
    list(
      input = rmd_knit,
      output_file = html_out,
      output_format = rmarkdown::html_document(),
      clean = FALSE 
    )
  )

  log_debug("pub_rmd_to_html: output: {html_out}")
  invisible(html_out)
}

#' Convert an rmd to an html article
#' 
#' @param rmd_in input Rmd file
#' @param rmd_out output Rmd file (optional)
#' @param article_name the name of the vignette
#' @export
pub_rmd_to_article <- function(rmd_in, rmd_out = NULL, article_name = NULL) {
  log_debug("pub_rmd_to_article: input: {rmd_in}")

  rmd_out <- rmd_out %||% tempfile(fileext = ".Rmd")

  vid <- mty_uuid() %>%
    stringr::str_sub(-4, -1)

  article_name <- article_name %||% sprintf("article_%s", vid)
  vig_path <- here::here("vignettes")
  vig_link <- fs::path_join(c(vig_path, glue("{article_name}.Rmd")))
  
  rmd_output <- preknit(
    rmd_in,
    rmd_out,
    "html"
  )
  log_debug("pub_rmd_to_article: created file: {rmd_out}")

  if (fs::file_exists(vig_link)) {
    fs::file_delete(vig_link)
  }
  fs::link_create(rmd_out, vig_link, symbolic = FALSE)
  log_debug("pub_rmd_to_article: created file: {vig_link}")

  pkgdown::build_article(article_name)

  invisible(NULL)
}
