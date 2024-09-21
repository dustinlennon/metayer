
# regular expressions for encode / decode functions
rexp <- "^```{(r.*)}$"
sexp <- "^``` {.r .knitr-(.*)}$"

#' Internal:  is this the start of a knitr chunk?
#' 
#' @keywords internal
#' @param l the line to test
is_knitr_chunk_start <- function(l) grepl(rexp, l, perl = TRUE)

#' Internal:  is this the start of an encoded knitr chunk?
#' 
#' @keywords internal
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
#' @keywords internal
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
#' N.B., this is a convenient place to inject into the knitr chunk header, e.g.,
#' metayer_hook
#' @keywords internal
#' @inheritParams encode_knitr
decode_knitr <- function(enc, dict) {
  key <- sub(sexp, "\\1", enc, perl = TRUE)
  val <- dict[[key]]

  sprintf("```{%s}", val)
}

#' Extract YAML from ipynb
#' 
#' Returns a nested list comprised of merged YAML from raw notebook cells.
#' 
#' @keywords internal
#' @param ipynb an ipynb file
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

#' Knit an rmd file before handing it off to a downstream publishing stage.
#' 
#' @param rmd_in input Rmd file
#' @param md_out output md file
#' @param root_dir knitr root.dir
#' @param requested_format currently, "html" or "pdf"
#' @export
preknit <- function(
    rmd_in,
    md_out = NULL,
    root_dir = NULL,
    requested_format = c("html", "pdf")) {

  log_debug("preknit: input: {rmd_in}")
  md_out <- md_out %||% tempfile(fileext = ".md")

  callr_env <- c(
    as.list(callr::rcmd_safe_env()),
    R_CONFIG_ACTIVE = Sys.getenv("R_CONFIG_ACTIVE")
  ) %>%
    unlist()

  callr::r_safe(
    function(input, output, fmt, root_dir) {
      library(magrittr)
      glue::glue("subprocess: getwd: {getwd()}") %>% cat(., "\n")
      cfg_pth <- here::here("config.yml")
      glue::glue("subprocess: config.yml: {cfg_pth}") %>% cat(., "\n")

      devtools::load_all()
      options(knitr.chunk.metayer_hook = TRUE)
      knitr::knit_hooks$set(metayer_hook = knitr_metayer_hook)
      knitr::opts_knit$set(metayer_pandoc_to = fmt, root.dir = root_dir)
      knitr::knit(input, output)
    },
    args = list(
      input = rmd_in,
      output = md_out,
      fmt = requested_format,
      root_dir = root_dir
    ),
    env = callr_env
  )

  log_debug("preknit: output: {md_out}")
  invisible(md_out)
}
