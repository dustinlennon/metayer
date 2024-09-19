#' Wrap png_data in an HTML img wrapper
#' 
#' @keywords internal
#' @param png_data raw png data
#' @returns an IMG element
pimg_as_html_data <- function(png_data, alt = NULL) {
  b64enc <- base64enc::base64encode(png_data)
  args <- c(
    src = glue::glue("data:image/png;base64, {b64enc}"),
    alt = alt
  ) %>%
    as.list()

  do.call(htmltools::img, args) %>%
    as.character()
}

#' @keywords internal
pimg_write_data <- function(data, pth) {
  pth_dir <- fs::path_dir(pth)

  if (!fs::dir_exists(pth_dir)) {
    cli_inform("pimg_write_data: creating directory: {pth_dir}")
    fs::dir_create(pth_dir)
  }

  cli_inform("pimg_write_data: writing file: {pth}")
  writeBin(data, pth)
}

#' @keywords internal
pimg_as_html_link <- function(img_data, src, alt = NULL) {
  pimg_write_data(img_data, src)
  args <- c(src = src, alt = alt) %>% as.list()
  do.call(htmltools::img, args) %>%
    as.character()
}

#' @keywords internal
pimg_as_md_link <- function(img_data, src, alt = NULL) {
  pimg_write_data(img_data, src)
  alt <- alt %||% ""
  sprintf("![%s](%s)", alt, src)
}
