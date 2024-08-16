
#' regular expressions for encode / decode functions
rexp <- "^```{(r.*)}$"
sexp <- "^``` {.r .knitr-(.*)}$"

#' Internal:  is this the start of a knitr chunk?
#' 
#' @param l the line to test
is_knitr <- function(l) grepl(rexp, l, perl = TRUE)

#' Internal:  is this the start of an encoded knitr chunk?
#' 
#' @param l the line to test
is_knitr_enc <- function(l) grepl(sexp, l, perl = TRUE)


#' Internal: encode the start of a knitr chunk as a hash
#' 
#' @param l the line to encode
#' @param dict an environment to use as a dictionary
encode_knitr <- function(l, dict) {
  val <- sub(rexp, "\\1", l, perl = TRUE)
  key <- hash(val)

  env_poke(dict, key, val)

  sprintf(
    "``` {.r .knitr-%s}",
    key
  )
}

#' Internal: decode the start of an encoded knitr chunk
#' 
#' @inheritParams encode_knitr
decode_knitr <- function(l, dict) {
  key <- sub(sexp, "\\1", l, perl = TRUE)
  val <- dict[[key]]

  chunk_id <- hash(uuid::UUIDgenerate())
  sprintf("```{%s metayer=\"%s\"}", val, chunk_id)
}

#' postprocess output of rmarkdown::convert_ipynb
#' 
#' This allows jupyter notebooks to inject YAML that overrides the usual convert_ipynb
#' defaults, namely, titles like:
#' 
#'   "An R Markdown document converted from "vignettes/examples.ipynb"
#' 
#' @param input input file, a jupyter notebooks
#' @param output output file, an Rmd file
#' @export
postprocess_rmd <- function(input, output = xfun::with_ext(input, "Rmd")) {
  tmp_src <- withr::local_tempfile()
  tmp_dest <- withr::local_tempfile()

  # postprocess
  dict <- new_environment()

  xfun::read_utf8(input) %>%
    purrr::modify_if(
      is_knitr,
      \(l) encode_knitr(l, dict)
    ) %>%
    xfun::write_utf8(tmp_src)

  rmarkdown::pandoc_convert(
    tmp_src,
    from = "markdown+yaml_metadata_block",
    to = "markdown",
    output = tmp_dest,
    options = c("--standalone")
  )

  xfun::read_utf8(tmp_dest) %>%
    purrr::modify_if(
      is_knitr_enc,
      \(l) decode_knitr(l, dict)
    ) %>%
    xfun::write_utf8(output)

  invisible(NULL)
}