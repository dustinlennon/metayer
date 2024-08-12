#' Initialize for common vignette settings
#' 
#' Called from onLoad
#' 
#' @keywords internal
#' @export
initialize_vignette <- function() {
  knitr::opts_knit$set(out.format = "html")
}

#' A refactored / shared code highlight function
#' 
#' @keywords internal
#' @param code the code, read from the source file
#' @param theme a knitr theme
highlight <- function(
    code,
    theme) {

  # if (!is.null(header)) {
  #   content <- sprintf("%s\n%s", header, content)
  # }

  # code <- content %>%
  #   highr::hi_html() %>%
  #   paste0(collapse = "\n")

  # tags <- htmltools::HTML(code) %>%
  #   htmltools::div(class = "source") %>%
  #   htmltools::pre() %>%
  #   htmltools::renderTags()

  # tags$html


  html <- highr::hi_html(code)

  hid <- hash(html) %>%
    hash_trim()

  # c.f., knitr:::theme_to_header_html

  css_file <- system.file("themes", sprintf("%s.css", theme), package = "knitr")
  css_raw <- xfun::read_utf8(css_file)
  css_obj <- knitr_css.parser(lines = css_raw)
  bgcolor <- css_obj$background$color

  css_bg <- sprintf("#div-%s .inline, .source {\n background-color: %s;\n}", hid, bgcolor)

  css_hl <- gsub(
    "^([.][a-z]{3} )",
    sprintf("#div-%s .hl\\1", hid),
    css_raw[-(1:3)]
  ) %>%
    paste(collapse = "\n")
  
  style <- c("<style>", css_bg, css_hl, "</style>") %>% paste0(collapse = "\n")

  tags <- htmltools::pre(
    htmltools::div(
      htmltools::HTML(style),
      htmltools::HTML(html),
      id = stringr::str_glue("div-{hid}"),
      class = "source"
    ),
  ) %>%
    htmltools::renderTags()

  tags$html
}

#' Wraps utils::capture.output for uniformity across publishing context.
#' 
#' @param obj an R object
#' @export
display_text <- function(obj) {
  out <- withr::with_options(
    list(
      cli.num_colors = 1
    ),
    {
      utils::capture.output(obj)
    }
  ) %>% 
    paste0(collapse = "\n")

  cli_alert("cli: called display_text")
  log_info("log: called display_text")

  if (isTRUE(getOption("knitr.in.progress"))) {
    cli_alert("knitr context")
    html <- sprintf("<pre>%s</pre>", out)
    rmarkdown::html_notebook_output_html(html)
  } else if (isTRUE(getOption("jupyter.in_kernel"))) {
    cli_alert("jupyter context")
    IRdisplay::display_text(out)
  } else {
    cli_alert("unknown context")
    # cli_abort("unknown execution context")
    NULL
  }
}

#' Internal: is the cell tagged as "yaml"
#' 
#' @keywords internal
#' @param c the cell to test
is_header_cell <- function(c) "yaml" %in% c$metadata$tags

#' Internal: coalesce lists
#' 
#' @keywords internal
#' @param agg the base case list 
#' @param y the new list to coalesce into the base case list
#' @returns a coalesced list
list_coalesce <- function(agg, y) {
  l <- c(agg, y)
  l[!duplicated(names(l), fromLast = TRUE) | (names(l) == "")]
}

#' Internal:  extract tagged yaml cells from ipython notebook
#' 
#' @keywords internal
#' @param content the json extracted content of a jupyter notebook
#' @returns the coalesced YAML from "yaml" tagged cells
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

#' Internal:  extract jupyter notebook cells
#' 
#' @keywords internal
#' @param content the json extracted content of a jupyter notebook
extract_jpy_cells <- function(content) {
  content$cells %>%
    purrr::discard(is_header_cell)
}

#' Internal:  construct an R markdown file
#' 
#' @keywords internal
#' @param cfg a config
#' @param content the json extracted content of a jupyter notebook
#' @param rmd_pth the r markdown path
construct_rmd_file <- function(cfg, content, rmd_pth) {
  
  # Create the Rmd file
  local({
    tmp <- withr::local_tempfile()
    jdata <- jsonlite::toJSON(content)
    readr::write_lines(jdata, tmp)
    rmarkdown::convert_ipynb(tmp, rmd_pth)
  })
  
  # withr::with_tempfile("tf", {
  #   jdata <- jsonlite::toJSON(content)
  #   readr::write_lines(jdata, tf)
  #   rmarkdown::convert_ipynb(tf, rmd_pth)
  # })

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

#' Internal:  render an HTML from R markdown file
#' 
#' @keywords internal
#' @param rmd_pth the rmd file name
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

#' Build a vignette from a jupyter notebook
#' 
#' Jupyter notebooks should be stored in ./vignettes directory.
#' 
#' @param ipynb_name the ipython notebook name
#' @export
build_vignette <- function(ipynb_name) {
  nb_pth <- here::here("vignettes", ipynb_name)
  rmd_pth <- xfun::with_ext(nb_pth, ".Rmd")

  content <- jsonlite::read_json(nb_pth)

  # partition cells
  cfg <- extract_yaml_cells(content)
  content$cells <- extract_jpy_cells(content)

  # export to Rmd
  construct_rmd_file(cfg, content, rmd_pth)
  render_rmd_file(rmd_pth)
}

#' Internal: a template for source files
#' @keywords internal
source_header_template <- function() {
  header <- "
#
# File:  {file_name}
#

"
  header
}

#' Internal: a template for source files
#' @keywords internal
source_footer_template <- function() {
  footer <- "  
"

  footer
}

#' Get fileset from a file name or directory
#' 
#' @keywords internal
#' @param path a file path, a directory path; if relative, assummed to be wrt the package root
get_fileset <- function(path) {
  if (!fs::is_absolute_path(path)) {
    path <- here::here(path)
  }

  path <- fs::path_real(path)

  if (fs::is_dir(path)) {
    file_names <- fs::dir_ls(
      path,
      type = c("file", "symlink"),
      regexp = "*\\.[Rr]$"
    )
  } else if (fs::is_file(path)) {
    file_names <- path
  } else {
    cli_abort(
      "unavailable path: {path}",
      .class = "file-not-found-error"
    )
  }

  file_names
}

#' Assemble codes to be highlighted
#' 
#' @keywords internal
#' @param args a fileset; or raw source code
#' @param method either "fileset" or "raw"
highlight_code_method <- function(args, method = c("fileset", "raw")) {
  method <- match.arg(method)
  switch(
    method,
    fileset = {
      get_fileset(args) %>%
        purrr::set_names() %>%
        purrr::map(xfun::read_utf8) %>%
        purrr::imap(
          \(v, file_name) {
            paste(
              stringr::str_glue(source_header_template()),
              v,
              source_footer_template(),
              sep = "\n",
              collapse = "\n"
            )
          }
        )
    },
    raw = args,
    {
      cli_abort("unknown method supplied: {method}")
    }
  )
}

#' Internal function
#' 
#' @keywords internal
#' @param ... passed to css.parser
knitr_css.parser <- function(...) {
  # workaround for ::: warnings in R CMD check
  css.parser <- get("css.parser", envir = asNamespace("knitr"))
  css.parser(...)
}

#' Highlight R source files and inject HTML into document
#' 
#' @keywords internal
#' @param args depends on method
#' @param theme a knitr css theme
#' @param method the method to use
highlight_source <- function(
    args,
    theme = "seashell",
    method = c("fileset", "raw")) {

  codes <- highlight_code_method(args, method = method)
  
  hl_codes <- codes %>%
    purrr::map(highr::hi_html) %>%
    lapply(paste0, collapse = "\n")

  pre_blocks <- hl_codes %>%
    purrr::map(
      \(h) {
        htmltools::div(
          htmltools::HTML(h),
          class = "source"
        )    
      }
    ) %>%
    purrr::map_vec(
      \(l) sprintf("<pre>%s</pre>", l)
    ) %>%
    htmltools::HTML()

  hid <- uuid::UUIDgenerate() %>% 
    hash() %>%
    hash_trim()

  # c.f., knitr:::theme_to_header_html
  css_file <- system.file("themes", sprintf("%s.css", theme), package = "knitr")
  css_raw <- xfun::read_utf8(css_file)
  css_obj <- knitr_css.parser(lines = css_raw)
  bgcolor <- css_obj$background$color

  css_bg <- stringr::str_glue(
    "#div-{hid} .inline, #div-{hid} .source {{
       background-color: {bgcolor};
     }}
     #div-{hid} .source {{
       margin: 1em 0 0 0;
       padding: 1em;
     }}"
  )

  css_hl <- gsub(
    "^([.][a-z]{3} )",
    sprintf("#div-%s .hl\\1", hid),
    css_raw[-(1:3)]
  ) %>%
    paste(collapse = "\n")
  
  style <- c("<style>", css_bg, css_hl, "</style>") %>% paste0(collapse = "\n")

  tags <- htmltools::div(
    htmltools::HTML(style),
    pre_blocks,
    id = stringr::str_glue("div-{hid}")
  ) %>%
    htmltools::renderTags()

  html <- tags$html

  html
}

#' Highlight a source file or the source files in a directory
#' 
#' @param path a path
#' @param theme a knitr theme
#' @param raw if TRUE, 'path' will be interpreted as lines of code
#' @export
display_source <- function(path, theme = "seashell", raw = FALSE) {
  html <- if (raw == FALSE) {
    highlight_source(path, theme, method = "fileset")
  } else {
    highlight_source(path, theme, method = "raw")
  }
  publish_context(html)
}


#' Output the html in the correct publishing context
#' 
#' @keywords internal
#' @param html some html to publish
publish_context <- function(html) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    rmarkdown::html_notebook_output_html(html)
  } else if (isTRUE(getOption("jupyter.in_kernel"))) {
    IRdisplay::display_html(html)
  } else {
    cli_abort("unknown execution context")
  }
}
