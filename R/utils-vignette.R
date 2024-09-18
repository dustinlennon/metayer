# display text ----------------------------------------------------------------

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
    NULL
  }
}

# code highlighting -----------------------------------------------------------

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

#' A refactored / shared code highlight function
#' 
#' @keywords internal
#' @param code the code, read from the source file
#' @param theme a knitr theme
highlight <- function(
    code,
    theme) {

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
