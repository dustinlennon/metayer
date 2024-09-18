
#' regular expressions for encode / decode functions
rexp <- "^```{(r.*)}$"
sexp <- "^``` {.r .knitr-(.*)}$"

#' Internal:  is this the start of a knitr chunk?
#' 
#' @param l the line to test
is_knitr_chunk_start <- function(l) grepl(rexp, l, perl = TRUE)

#' Internal:  is this the start of an encoded knitr chunk?
#' 
#' @param l the line to test
is_knitr_enc <- function(l) grepl(sexp, l, perl = TRUE)


#' Internal: encode the start of a knitr chunk as a hash.
#' 
#' In order for pandoc to work as expected, it needs to interpret the content in 
#' the braces as class tags.  Note that this requires adding (and later removing) a
#' single space., e.g.
#' 
#'   ``` {.r .knitr-1234567890}
#'     ^^^
#' 
#' @param l the line to encode
#' @param dict an environment to use as a dictionary
encode_knitr <- function(l, dict, key = NULL) {
  val <- sub(rexp, "\\1", l, perl = TRUE)
  key <- key %||% hash(mty_uuid())

  env_poke(dict, key, val)

  sprintf(
    "``` {.r .knitr-%s}",
    key
  )
}

#' Internal: decode the start of an encoded knitr chunk
#' 
#' @inheritParams encode_knitr
decode_knitr <- function(enc, dict) {
  key <- sub(sexp, "\\1", enc, perl = TRUE)
  val <- dict[[key]]

  sprintf("```{%s metayer='%s'}", val, key)
}

#' postprocess output of rmarkdown::convert_ipynb
#' 
#' This is part of a multi-step process that converts jupyter notebooks into viable knitr
#' Rmd files.  
#' 
#' In particular, it introduces a per-cell unique identifier and allows the caller to
#' replace any existing YAML in the input.  The latter is useful because the existing
#' implementaton of rmarkdown::convert_ipynb produces an Rmd file with a fixed title 
#' and no mechanism for injecting additional, or modifying existing, YAML configuration.
#' 
#' @param input input file, an Rmd file as prepared by rmarkdown::convert_ipynb
#' @param output output file, an Rmd file
#' @param conf a nested list that will be converted into a YAML header
#' @export
process_rmd <- function(
    rmd_in,
    rmd_out,
    conf = list()) {

  dict <- new_environment()

  # add chunk ids and map knitr cell headers into a class
  rmd_enc <- tempfile()
  xfun::read_utf8(rmd_in) %>%
    purrr::modify_if(
      is_knitr_chunk_start,
      \(l) encode_knitr(l, dict)
    ) %>%
    xfun::write_utf8(rmd_enc)

  # remove any yaml metadata added upstream (e.g., fixed title from convert_ipynb)
  rmd_yaml <- tempfile()
  rmarkdown::pandoc_convert(
    rmd_enc,
    from = "markdown+yaml_metadata_block",
    to = "markdown",
    output = rmd_yaml
  )

  # combine user supplied config and markdown in a single file with YAML header
  header <- sprintf("---\n%s---\n", yaml::as.yaml(conf))
  xfun::write_utf8(header, rmd_out)
  xfun::read_utf8(rmd_yaml) %>% 
    purrr::modify_if(
      is_knitr_enc,
      \(l) decode_knitr(l, dict)
    ) %>%
    xfun::append_utf8(rmd_out, sort = FALSE)

  invisible(NULL)
}

#' Extract YAML from ipynb
#' 
#' Returns a nested list comprised of merged YAML from raw notebook cells.
#' 
#' @param ipynb an ipynb file
#' @export
ipynb_yaml_extract <- function(ipynb) {
  json <- jsonlite::read_json(ipynb)  
  cells <- json$cells %||% list()

  yaml_blocks <- cells %>%
    purrr::keep(
      \(cell) cell$cell_type == "raw"
    ) %>%
    purrr::map(
      \(cell) {
        cell$source %>%
          stringr::str_trim(side = "right")
      }
    ) %>%
    purrr::keep(
      \(lines) {
        dplyr::first(lines) == "---" &&
          dplyr::last(lines) == "---"
      }
    ) %>%
    purrr::map(
      \(src) paste0(src, collapse = "\n") 
    ) %>%
    purrr::map(
      yaml::yaml.load
    )

  purrr::reduce(yaml_blocks, update_list)
}
