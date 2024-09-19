#' Convert ipynb to knitr-read rmd
#' 
#' @param ipynb the ipynb filename
#' @param rmd the rmd filename
#' @export
pub_ipynb_to_rmd <- function(ipynb, rmd) {
  conf <- ipynb_yaml_extract(ipynb)

  doc_start <- tempfile()
  rmarkdown::convert_ipynb(input = ipynb, output = doc_start)

  process_rmd(doc_start, rmd, conf = conf)
}

#' Convert an rmd to a pdf
#' 
#' @param rmd the rmd filename
#' @param .pdf the pdf filename
#' @export
pub_rmd_to_pdf <- function(rmd, .pdf) {
  doc <- rmarkdown::pdf_document()
  doc$knitr$knit_hooks$metayer <- hook_metayer

  xfun::Rscript_call(
    rmarkdown::render,
    list(
      input = rmd,
      output_file = .pdf,
      output_format = doc,
      clean = FALSE 
    )
  )  
}

#' Convert an rmd to an html
#' 
#' @param rmd the rmd filename
#' @param html the pdf filename
#' @export
pub_rmd_to_html <- function(rmd, html) {
  doc <- rmarkdown::html_document()
  doc$knitr$knit_hooks$metayer <- hook_metayer

  xfun::Rscript_call(
    rmarkdown::render,
    list(
      input = rmd,
      output_file = html,
      output_format = doc,
      clean = FALSE 
    )
  )  
}

#' Convert an rmd to an html
#' 
#' @param rmd the rmd filename
#' @export
pub_rmd_to_article <- function(rmd) {

  basename <- fs::path_file(rmd)
  stub <- basename %>%
    fs::path_ext_remove()
    
  link <- fs::path_join(c(
    here::here("vignettes"),
    basename
  ))

  if (link != rmd) {
    if (fs::file_exists(link)) {
      fs::file_delete(link)
    }
    fs::link_create(rmd, link, symbolic = FALSE)
  }

  with_monkey_patch(
    "rmarkdown::html_document",
    wrapped_doc,
    {
      pkgdown::build_article(stub, lazy = FALSE, quiet = FALSE, new_process = FALSE)
    }
  )
}

build_site <- function() {
  with_monkey_patch(
    "rmarkdown::html_document",
    wrapped_doc,
    {
      pkgdown::build_site(devel = TRUE, lazy = TRUE)
    }
  )
}
