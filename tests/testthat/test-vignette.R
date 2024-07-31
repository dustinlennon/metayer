test_that("display_source themes are applied on a per-cell basis", {
  test_sanitize()

  knitr::opts_knit$set(out.format = "html")

  theme <- "pablo"
  args <- "./tests/testthat/fixtures/display_source"

  codes <- highlight_code_method(args, method = "fileset")

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
    stringr::str_sub(-4, -1)

  # c.f., knitr:::theme_to_header_html

  css_file <- system.file("themes", sprintf("%s.css", theme), package = "knitr")
  css_raw <- xfun::read_utf8(css_file)
  css_obj <- knitr:::css.parser(lines = css_raw)
  bgcolor <- css_obj$background$color

  css_bg <- sprintf(
    "#div-%s .inline, #div-%s .source {
       background-color: %s;       
     }
     #div-%s .source {
       margin: 1em 0 0 0;
     }", hid, hid, bgcolor, hid)

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

})


test_that("display_highlight themes are applied on a per-cell basis", {

  test_sanitize()

  knitr::opts_knit$set(out.format = "html")

  theme <- "pablo"
  path <- "./tests/testthat/fixtures/display_source/foo.r"
  args <- xfun::read_utf8(path)

  highlight_source(args, "seashell", method = "raw")

  codes <- highlight_code_method(args, method = "raw")

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
    stringr::str_sub(-4, -1)

  # c.f., knitr:::theme_to_header_html

  css_file <- system.file("themes", sprintf("%s.css", theme), package = "knitr")
  css_raw <- xfun::read_utf8(css_file)
  css_obj <- knitr:::css.parser(lines = css_raw)
  bgcolor <- css_obj$background$color

  css_bg <- sprintf(
    "#div-%s .inline, #div-%s .source {
       background-color: %s;       
     }
     #div-%s .source {
       margin: 1em 0 0 0;
     }", hid, hid, bgcolor, hid)

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


})