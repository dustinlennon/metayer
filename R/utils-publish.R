# regular expressions for encode / decode functions
rexp <- "^```{(r.*)}$"
sexp <- "^``` {.r .knitr-(.*)}$"

is_knitr_chunk_start <- function(l) grepl(rexp, l, perl = TRUE)
is_knitr_enc <- function(l) grepl(sexp, l, perl = TRUE)


#' encode the start of a knitr chunk as a hash.
#' 
#' In order for pandoc to work as expected, it needs to interpret the content in 
#' the braces as class tags.  Note that this requires adding (and later removing) a
#' single space.
#' 
#' @keywords internal
#' @param chunk_header the line to encode
#' @param dict an environment to use as a dictionary
encode_knitr <- function(chunk_header, dict, key = NULL) {
  val <- sub(rexp, "\\1", chunk_header, perl = TRUE)
  key <- key %||% hash(mty_uuid())

  env_poke(dict, key, val)

  sprintf(
    "``` {.r .knitr-%s}",
    key
  )
}

#' decode the start of an encoded knitr chunk
#' 
#' N.B., this is a convenient place to inject into the knitr chunk header, e.g.,
#' metayer_hook
#' @keywords internal
#' @inheritParams encode_knitr
decode_knitr <- function(enc, dict) {
  key <- sub(sexp, "\\1", enc, perl = TRUE)
  chunk_header <- dict[[key]]

  sprintf("```{%s}", chunk_header)
}

#' extract YAML from a notebook
#' 
#' Returns a nested list comprised of merged YAML from raw notebook cells.
#' 
#' @keywords internal
#' @param ipynb_in an ipynb file
ipynb_yaml_extract <- function(ipynb_in) {
  json <- jsonlite::read_json(ipynb_in)  
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
    )

  purrr::reduce(yaml_blocks, update_list) %>%
    yaml::yaml.load(
      handlers = list(
        with_env = yaml_handler_with_env
      )
    )
}

#' prepare rmarkdown articles
#' 
#' Prepare rmarkdown articles for downstream processing.  
#' 
#' @param ... the path for Rmd files, passed to here::here
#' @param regexp a regular expression for filtering files
#' @param copy_to a destination directory
#' @export
prep_articles <- function(
    ...,
    regexp = "*ipynb",
    copy_to = NULL) {

  rootdir <- here::here(...)
  candidate_files <- fs::dir_ls(rootdir, regexp = regexp)

  idx <- candidate_files %>%
    fs::path_file() %>%
    stringr::str_starts("_", negate = TRUE)

  allowed_files <- candidate_files[idx]

  for (src in allowed_files) {
    dst <- fs::path_ext_set(src, ".Rmd")

    if (fs::path_ext(src) != "ipynb") {
      cli_warn("{src} is not an ipynb file")
      next
    }

    if (!is_null(copy_to)) {
      if (fs::dir_exists(copy_to) == FALSE) {
        cli_abort("prep_articles: path not found: {dst}")
      }

      dst <- fs::path_join(c(
        copy_to,
        fs::path_file(dst)
      )) %>%
        fs::path_norm()
    }

    pub_ipynb_to_rmd(src, dst)
  }
}

#' build the site with envvars
#' 
#' @keywords internal
#' @export
build_site <- function() {
  prep_articles("vignettes")

  withr::with_envvar(
    list(
      R_CONFIG_ACTIVE = "jupyter"
    ),
    {
      pkgdown::build_site(preview = FALSE)
    }
  )
}

#' build the site news
#' 
#' @keywords internal
#' @export
build_news <- function() {
  withr::with_envvar(
    list(
      R_CONFIG_ACTIVE = "jupyter"
    ),
    {
      pkgdown::build_news()
    }
  )
}

#' quick wrap a vignette for jupyter config
#' 
#' @keywords internal
#' @param code client code
#' @param .envir env in which to evaluate client code
#' @export
jpy_wrap <- function(code, .envir = parent.frame()) {
  withr::with_envvar(
    list(
      R_CONFIG_ACTIVE = "jupyter"
    ),
    eval(substitute(code), .envir)
  )
}
