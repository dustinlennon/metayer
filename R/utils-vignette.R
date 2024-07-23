#' Set the highlight theme; output HTML
#' 
#' Choose from published themes:  knitr::knit_theme$get()
#'   preview:  http://animation.r-forge.r-project.org/knitr/
#' 
#' This needs to operate in two modes.  One is within Jupyter; the other is
#' when building the html.
#' 
#' @param theme a published theme name
#' @export
set_highlight_theme <- function(theme) {
  knitr::opts_knit$set(out.format = "html")
  css <- sprintf(
    "<style>%s</style>",
    knitr::knit_theme$get(theme)$highlight
  )

  if (isTRUE(getOption("knitr.in.progress"))) {
    rmarkdown::html_notebook_output_html(css)
  } else if (isTRUE(getOption("jupyter.in_kernel"))) {
    IRdisplay::display_html(css)
  } else {
    log_abort("unknown execution context")
  }
}

#' Initialize for common vignette settings
#' 
#' Called from onLoad
#' 
#' @param disable_cli disable cli, a boolean
#' @param highlight_theme the code highlight theme
#' @export
initialize_vignette <- function(
  highlight_theme = "seashell"
) {
  set_highlight_theme(highlight_theme)
}

#' A refactored / shared code highlight function
highlight <- function(
    content,
    header = NULL) {

  if (!is.null(header)) {
    content <- sprintf("%s\n%s", header, content)
  }

  code <- content %>%
    highr::hi_html() %>%
    paste0(collapse = "\n")

  tags <- htmltools::HTML(code) %>%
    htmltools::div(class = "source") %>%
    htmltools::pre() %>%
    htmltools::renderTags()

  tags$html
}

display_highlight <- function(
    content,
    header = NULL) {

  html <- highlight(content, header)

  if (isTRUE(getOption("knitr.in.progress"))) {
    rmarkdown::html_notebook_output_html(html)
  } else if (isTRUE(getOption("jupyter.in_kernel"))) {
    IRdisplay::display_html(html)
  } else {
    log_abort("unknown execution context")
  }
}

dms_header_template <- "
#
# File:  {file_name}
#

"

# display_module_source <- function(root_path, module_name) {
#   loc <- locator_factory(root_path, module_name)()

#   htmls <- list()
#   for (file_name in loc$module_files) {
#     header <- stringr::str_glue(dms_header_template)
#     html <- here::here(file_name) %>%
#       xfun::file_string() %>%
#       highlight(header)

#     htmls[[file_name]] <- html
#   }

#   html <- paste(htmls, collapse = "\n")

#   if (isTRUE(getOption("knitr.in.progress"))) {
#     rmarkdown::html_notebook_output_html(html)
#   } else if (isTRUE(getOption("jupyter.in_kernel"))) {
#     IRdisplay::display_html(html)
#   } else {
#     log_abort("unknown execution context")
#   }

# }

is_header_cell <- function(c) "yaml" %in% c$metadata$tags

list_coalesce <- function(agg, y) {
  l <- c(agg, y)
  l[!duplicated(names(l), fromLast = TRUE) | (names(l) == "")]
}

extract_yaml_cells <- function(content) {
  # Extract any YAML cells and coalesce
  content$cells %>%
    purrr::keep(is_header_cell) %>%
    purrr::map(
      \(x) magrittr::use_series(x, "source")
    ) %>%
    purrr::map(
      \(x) paste0(x, collapse = "\n")
    ) %>%
    purrr::map(
      yaml::yaml.load
    ) %>%
    purrr::reduce(
      \(x, y) list_coalesce(x, y),
      .init = list()
    )
}

extract_jpy_cells <- function(content) {
  content$cells %>%
    purrr::discard(is_header_cell)
}

construct_rmd_file <- function(cfg, content, rmd_pth) {
  # Create the Rmd file
  withr::with_tempfile("tf", {
    jdata <- jsonlite::toJSON(content)
    readr::write_lines(jdata, tf)
    rmarkdown::convert_ipynb(tf, rmd_pth)
  })

  # Replace the front matter YAML
  lines <- readr::read_lines(rmd_pth)
  found_header <- FALSE
  skipping <- FALSE
  idx <- rep_len(TRUE, length(lines))
  for (i in seq_along(lines)) {
    line <- lines[i]
    if (line == "---" && found_header == FALSE) {
      skipping <- TRUE
      found_header <- TRUE
      idx[i] <- FALSE
      next
    } else if (line == "---" && skipping == TRUE) {
      skipping <- FALSE
      idx[i] <- FALSE
      break
    }

    if (skipping) {
      idx[i] <- FALSE
    }
  }

  # build the new doc
  rmd_doc <- c(
    "---",
    yaml::as.yaml(cfg) %>%
      stringr::str_trim(),
    "---\n",
    lines[idx]
  ) %>%
    paste0(collapse = "\n")

  readr::write_lines(rmd_doc, rmd_pth)
}

render_rmd_file <- function(rmd_pth) {

  output_format <- rmarkdown::html_document(
    keep_md = TRUE
  )

  withr::with_options(
    list(
      width = 108
    ),
    rmarkdown::render(
      rmd_pth,
      output_format = output_format
    )
  )
}

build_vignette <- function(jnb_name) {
  nb_pth <- here::here("vignettes", jnb_name)
  rmd_pth <- xfun::with_ext(nb_pth, ".Rmd")

  content <- jsonlite::read_json(nb_pth)

  # partition cells
  cfg <- extract_yaml_cells(content)
  content$cells <- extract_jpy_cells(content)

  # export to Rmd
  construct_rmd_file(cfg, content, rmd_pth)
  render_rmd_file(rmd_pth)
}

display_text <- function(obj) {
  cobj <- as.character(obj) %>%
    paste0(collapse = "\n")

  if (isTRUE(getOption("knitr.in.progress"))) {
    cobj
  } else if (isTRUE(getOption("jupyter.in_kernel"))) {
    IRdisplay::display_text(cobj)
  } else {
    log_abort("unknown execution context")
  }
  invisible(obj)
}

#' Highlighted R source files and inject HTML into document
#' 
#' @param path a path to a source file or directory
#' @export
display_source <- function(path) {

  if (!fs::is_absolute_path(path)) {
    path <- here::here(path)
  }

  path <- fs::path_real(path)

  log_alert_info("sourcing from {path}")

  if (fs::is_dir(path)) {
    file_names <- fs::dir_ls(
      path,
      type = c("file", "symlink"),
      glob = "\\.[Rr]$",
    )
  } else if (fs::is_file(path)) {
    file_names <- path
  } else {
    log_abort(
      "unavailabe path: {path}",
      .class = "file-not-found-error"
    )
  }

  # generate highlighted HTML
  htmls <- list()
  for (file_name in file_names) {
    header <- stringr::str_glue(dms_header_template)
    html <- here::here(file_name) %>%
      xfun::file_string() %>%
      highlight(header)

    htmls[[file_name]] <- html
  }

  html <- paste(htmls, collapse = "\n")

  if (isTRUE(getOption("knitr.in.progress"))) {
    rmarkdown::html_notebook_output_html(html)
  } else if (isTRUE(getOption("jupyter.in_kernel"))) {
    IRdisplay::display_html(html)
  } else {
    log_abort("unknown execution context")
  }

}
