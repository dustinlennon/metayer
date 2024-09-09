# initialization --------------------------------------------------------------

#' Initialize for common vignette settings
#' 
#' Called from onLoad
#' 
#' @keywords internal
#' @export
initialize_vignette <- function() {
  knitr::opts_knit$set(out.format = "html")
}

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
    # cli_abort("unknown execution context")
    NULL
  }
}

# custom convert_ipynb --------------------------------------------------------

# #' Internal: is the cell tagged as "yaml"
# #' 
# #' @keywords internal
# #' @param c the cell to test
# is_header_cell <- function(c) "yaml" %in% c$metadata$tags

# #' Internal: coalesce lists
# #' 
# #' @keywords internal
# #' @param agg the base case list 
# #' @param y the new list to coalesce into the base case list
# #' @returns a coalesced list
# list_coalesce <- function(agg, y) {
#   l <- c(agg, y)
#   l[!duplicated(names(l), fromLast = TRUE) | (names(l) == "")]
# }

# #' Internal:  extract tagged yaml cells from ipython notebook
# #' 
# #' @keywords internal
# #' @param content the json extracted content of a jupyter notebook
# #' @returns the coalesced YAML from "yaml" tagged cells
# extract_yaml_cells <- function(content) {
#   # Extract any YAML cells and coalesce
#   content$cells %>%
#     purrr::keep(is_header_cell) %>%
#     purrr::map(
#       \(x) magrittr::use_series(x, "source")
#     ) %>%
#     purrr::map(
#       \(x) paste0(x, collapse = "\n")
#     ) %>%
#     purrr::map(
#       yaml::yaml.load
#     ) %>%
#     purrr::reduce(
#       \(x, y) list_coalesce(x, y),
#       .init = list()
#     )
# }

# #' Internal:  extract jupyter notebook cells
# #' 
# #' @keywords internal
# #' @param content the json extracted content of a jupyter notebook
# extract_jpy_cells <- function(content) {
#   content$cells %>%
#     purrr::discard(is_header_cell)
# }

# #' Internal:  construct an R markdown file
# #' 
# #' @keywords internal
# #' @param cfg a config
# #' @param content the json extracted content of a jupyter notebook
# #' @param rmd_pth the r markdown path
# construct_rmd_file <- function(cfg, content, rmd_pth) {
  
#   # Create the Rmd file
#   local({
#     tmp <- withr::local_tempfile()
#     jdata <- jsonlite::toJSON(content)
#     readr::write_lines(jdata, tmp)
#     rmarkdown::convert_ipynb(tmp, rmd_pth)
#   })
  
#   # withr::with_tempfile("tf", {
#   #   jdata <- jsonlite::toJSON(content)
#   #   readr::write_lines(jdata, tf)
#   #   rmarkdown::convert_ipynb(tf, rmd_pth)
#   # })

#   # Replace the front matter YAML
#   lines <- readr::read_lines(rmd_pth)
#   found_header <- FALSE
#   skipping <- FALSE
#   idx <- rep_len(TRUE, length(lines))
#   for (i in seq_along(lines)) {
#     line <- lines[i]
#     if (line == "---" && found_header == FALSE) {
#       skipping <- TRUE
#       found_header <- TRUE
#       idx[i] <- FALSE
#       next
#     } else if (line == "---" && skipping == TRUE) {
#       skipping <- FALSE
#       idx[i] <- FALSE
#       break
#     }

#     if (skipping) {
#       idx[i] <- FALSE
#     }
#   }

#   # build the new doc
#   rmd_doc <- c(
#     "---",
#     yaml::as.yaml(cfg) %>%
#       stringr::str_trim(),
#     "---\n",
#     lines[idx]
#   ) %>%
#     paste0(collapse = "\n")

#   readr::write_lines(rmd_doc, rmd_pth)
# }

# #' Internal:  render an HTML from R markdown file
# #' 
# #' @keywords internal
# #' @param rmd_pth the rmd file name
# render_rmd_file <- function(rmd_pth) {
#   output_format <- rmarkdown::html_document(
#     keep_md = TRUE
#   )

#   withr::with_options(
#     list(
#       width = 108
#     ),
#     rmarkdown::render(
#       rmd_pth,
#       output_format = output_format
#     )
#   )
# }

# #' Build a vignette from a jupyter notebook
# #' 
# #' Jupyter notebooks should be stored in ./vignettes directory.
# #' 
# #' @param ipynb_name the ipython notebook name
# #' @export
# build_vignette <- function(ipynb_name) {
#   nb_pth <- here::here("vignettes", ipynb_name)
#   rmd_pth <- xfun::with_ext(nb_pth, ".Rmd")

#   content <- jsonlite::read_json(nb_pth)

#   # partition cells
#   cfg <- extract_yaml_cells(content)
#   content$cells <- extract_jpy_cells(content)

#   # export to Rmd
#   construct_rmd_file(cfg, content, rmd_pth)
#   render_rmd_file(rmd_pth)
# }

# file system -----------------------------------------------------------------

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

# plot context  ---------------------------------------------------------------

#' Get the grdev operator
#' 
#' @param gr_dev the device type
get_grdev_op <- function(gr_dev = c("png", "pdf", "x11")) {
  gr_op <- switch(tolower(gr_dev),
    png = png,
    pdf = pdf,
    x11 = x11,
    NULL
  )

  if (is.null(gr_op)) {
    mty_abort("Unknown value for gr_dev: {gr_dev}")
  } 

  gr_op
}

#' Get the grdev extension
#' 
#' @param gr_dev the device type
get_grdev_ext <- function(gr_dev = c("png", "pdf", "x11")) {
  gr_ext <- switch(tolower(gr_dev),
    png = ".png",
    pdf = ".pdf",
    x11 = "",
    NULL
  )

  if (is.null(gr_ext)) {
    mty_abort("Unknown value for gr_dev: {gr_dev}")
  } 

  gr_ext
}

#' Return the default close_device value
#' 
#' @param gr_dev the device type
#' @param passed_args a list of passed args
get_grdev_close_device <- function(gr_dev = c("png", "pdf", "x11")) {
  status <- switch(tolower(gr_dev),
    png = TRUE,
    pdf = TRUE,
    x11 = FALSE,
    NULL
  )

  if (is.null(status)) {
    mty_abort("Unknown value for gr_dev: {gr_dev}")
  } 

  status
}

#' Adapt the args in the call for a particular graphical device
#' 
#' @param gr_dev the device type
#' @param passed_args a list of passed args
adapt_grdev_args <- function(
    gr_dev,
    passed_args) {

  gr_op <- get_grdev_op(gr_dev)

  allowed_args <- names(formals(gr_op))
  gr_args <- passed_args[names(passed_args) %in% allowed_args]
  gr_args
}

#' Adapt the file and filename parameters
#' 
#' @param gr_dev the device type
#' @param passed_args a list of passed args
adapt_grdev_file <- function(
    gr_dev,
    passed_args) {

  gr_ext <- get_grdev_ext(gr_dev)

  passed_args$file <- passed_args$file %||% passed_args$filename %||% tempfile()
  passed_args$filename <- passed_args$filename %||% passed_args$file

  # log_info("adapt_grdev_file:")
  # log_info("file: {passed_args$file %||% '<null>'}")
  # log_info("filename: {passed_args$filename %||% '<null>'}")

  passed_args$file <- fs::path_ext_set(passed_args$file, gr_ext) %>%
    as.character()

  passed_args$filename <- fs::path_ext_set(passed_args$filename, gr_ext) %>%
    as.character()

  if (passed_args$file != passed_args$filename) {
    mty_abort("mismatched parameters: {passed_args$file %||% '<null>'} {passed_args$filename %||% '<null>'}")
  }

  passed_args$.file <- passed_args$file
  passed_args
}

#' Provide a graphics device context
#' 
#' @param code plotting code
#' @param gr_dev the name of the graphics device
#' @inheritParams grDevices::png
#' @inheritParams grDevices::pdf
#' @inheritParams grDevices::x11
#' @param ... graphical parameters
#' @param .close_device a boolean, if TRUE, dev.close() is called
#' @param .file a filename, overrides any passed_args
#' @param .envir the environment in which code is evaluated
#' @returns rendered data, if available
#' @export
with_grdev <- function(
    code,
    gr_dev,
    antialias = NULL,
    bg = NULL,
    canvas = NULL,
    colormodel = NULL,
    compress = NULL,
    display = NULL,
    encoding = NULL,
    family = NULL,
    fg = NULL,
    file = NULL,
    filename = NULL,
    fillOddEven = NULL,
    fonts = NULL,
    gamma = NULL,
    height = 7,
    onefile = NULL,
    pagecentre = NULL,
    paper = NULL,
    pointsize = 12,
    res = 96,
    symbolfamily = NULL,
    title = NULL,
    type = NULL,
    units = NULL,
    useDingbats = NULL,
    useKerning = NULL,
    version = NULL,
    width = 7,
    xpos = NULL,
    ypos = NULL,
    ...,
    .close_device = NULL,
    .envir = parent.frame()) {

  code <- call("eval", substitute(code))

  passed_args <- call_match(defaults = TRUE) %>%
    as.list() %>%
    tail(-1) %>%
    purrr::discard(is.null)

  passed_args <- adapt_grdev_file(gr_dev, passed_args)
  gr_args <- adapt_grdev_args(gr_dev, passed_args)
  gr_op <- get_grdev_op(gr_dev)

  .close_device <- .close_device %||% get_grdev_close_device(gr_dev)

  log_info("with_grdev:")
  log_info(skip_formatter(
    capture.output(str(names(gr_args)))
  ))

  # ensure the directory exists
  fs::path_dir(passed_args$.file) %>%
    fs::dir_create()

  do.call(
    gr_op,
    gr_args
  )

  withr::with_par(
    list(...),
    {
      tryCatch(
        eval(code, envir = .envir),
        finally = {
          dev.flush()
          if (.close_device)
            dev.off()
        }
      )        
    }
  )    

  if (fs::file_exists(passed_args$.file)) {
    xfun::read_bin(passed_args$.file)
  } else {
    invisible(NULL)
  }
}

#' Provide a plot context that is independent of publishing context
#' 
#' @inheritParams with_grdev
#' @export
cfplot <- function(
    code, 
    antialias = NULL,
    bg = NULL,
    canvas = NULL,
    colormodel = NULL,
    compress = NULL,
    display = NULL,
    encoding = NULL,
    family = NULL,
    fg = NULL,
    file = NULL,
    filename = NULL,
    fillOddEven = NULL,
    fonts = NULL,
    gamma = NULL,
    height = 7,
    onefile = NULL,
    pagecentre = NULL,
    paper = NULL,
    pointsize = 12,
    res = 96,
    symbolfamily = NULL,
    title = NULL,
    type = NULL,
    units = "in",
    useDingbats = NULL,
    useKerning = NULL,
    version = NULL,
    width = 7,
    xpos = NULL,
    ypos = NULL,
    ...,
    .envir = parent.frame()) {

  passed_args <<- call_match(defaults = TRUE) %>%
    as.list() %>%
    purrr::discard(is.null) %>%
    tail(-1)

  rmarkdown_pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr_in_progress <- isTRUE(getOption("knitr.in.progress"))
  jupyter_in_kernel <- isTRUE(getOption("jupyter.in_kernel"))

  wrapper <- function(img_data) invisible(NULL)
  passed_args$file <- NULL
  passed_args$filename <- NULL

  if (jupyter_in_kernel) {
    cli_alert("cfplot: jupyter png")
    passed_args$gr_dev <- "png"
    wrapper <- wrap_png

  } else if (knitr_in_progress) {
    has_metayer_hook <- get0("has_metayer_hook", ifnotfound = FALSE)
    log_info("has_metayer_hook: {has_metayer_hook}")

    if (rmarkdown_pandoc_to == "html") {
      cli_alert("cfplot: knitr html")
      passed_args$gr_dev <- "png"
    } else {
      cli_alert("cfplot: knitr pdf")
      passed_args$gr_dev <- "pdf"
    }

    opts <- knitr::opts_current$get()
    chunk_id <- opts$metayer
    chunk_env <- storage_env("metayer", "chunks", chunk_id)

    passed_args$file <- with(
      opts,
      paste0(fig.path, label)
    ) %>% 
      fs::path_ext_set(
        get_grdev_ext(passed_args$gr_dev)
      )

    wrapper <- if (has_metayer_hook) {
      wrap_img_factory(chunk_id, passed_args$file)
    } else {
      wrap_png
    }

  } else {
    cli_alert("cfplot: default")
    passed_args$gr_dev <- "x11"
  }

  log_info("cfplot:")
  log_info(skip_formatter(
    capture.output(str(names(passed_args)))
  ))

  res <- do.call(with_grdev, passed_args)

  wrapper(res)
}

#' Wrap png_data in an HTML img wrapper
#' 
#' @param png_data raw png data
#' @returns an IMG element
wrap_png <- function(png_data) {
  b64enc <- base64enc::base64encode(png_data)
  htmltools::img(
    src = glue::glue("data:image/png;base64, {b64enc}")
  )
}

wrap_img_factory <- function(chunk_id, file_name) {
  function(img_data) {
    nb <- length(img_data)
    log_info("wrap_img_factory: {file_name} {nb}")

    chunk_env <- storage_env("metayer", "chunks", chunk_id)
    chunk_env$output <- sprintf("![](%s)", file_name)
  }
}

#' The metayer/knitr hook used for preprocessing / postprocessing
#' 
#' Refer to [knitr documentation](https://yihui.org/knitr/hooks)
#' 
#' @param before a boolean
#' @param options the current chunk options
#' @param envir the environment in which our code will be executed
#' @param name name associated with the hook, e.g. "metayer"
#' @param ... to match knitr hook signature
hook_metayer <- function(before, options, envir, name, ...) {
  if (before) {
    log_info("hook_metayer:  before")
    env_poke(envir, "has_metayer_hook", TRUE)
  } else {    
    log_info("hook_metayer:  after")

    chunk_env <- storage_env("metayer", "chunks", options$metayer)

    if (!is.null(chunk_env$output)) {
      options$results <- FALSE
      return(chunk_env$output)
    }
  }
  invisible(NULL)
}


# # # optimal figure extents ------------------------------------------------------

# #' Return optimized figure extents
# #' 
# #' Ref: https://predict-epfl.github.io/piqp/
# #' @param W the maximum figure width
# #' @param H the maximum figure height
# #' @param dx the diff of xlim
# #' @param dy the diff of ylim
# #' @param distortion_ratio the maximum allowable distortion ratio, i.e.: (w / h) / (dx / dy)
# opt_plotbox <- function(W, H, dx, dy, distortion_ratio = 2) {

#   if (distortion_ratio < 1) {
#     rlang::abort("distortion ratio error: {distortion_ratio} < 1")
#   }

#   Pm <- Matrix(
#     c(
#       0, -1,
#       -1, 0
#     ), 2, 2,
#     byrow = TRUE,
#     sparse = TRUE
#   )

#   cv <- NULL
#   Am <- NULL
#   bv <- NULL

#   a <- distortion_ratio
#   c0 <- dx / dy
#   Gm <- Matrix(
#     c(
#       -1, c0 / a,
#       1, -c0 * a
#     ), 2, 2,
#     byrow = TRUE,
#     sparse = TRUE
#   )
#   hv <- c(0, 0)

#   x_lb <- c(0, 0)
#   x_ub <- c(W, H)

#   settings <- list(verbose = FALSE)
#   model <- piqp::piqp(Pm, cv, Am, bv, Gm, hv, x_lb, x_ub, settings)

#   # Solve
#   res <- model$solve()
#   list(
#     w = res$x[1],
#     h = res$x[2],
#     drat = (res$x[1] / res$x[2]) / (dx / dy)
#   )

# }

# #' Create a reasonably sized figure given extents and constraints
# #' 
# #' @param xlim the x extents
# #' @param ylim the y extents
# #' @param code the code to run after calling the graphics device
# #' @param ... graphical parameters
# #' @param .pretty_axes use pretty to update specified xlim and ylim
# #' @param .max_width max figure width in inches
# #' @param .max_height max figure height in inches
# #' @param .max_distortion the maximum allowable distortion ratio
# #' @param .dpi the dots per inch
# #' @param .envir the execution environment in which to process code block
# aplt <- function(
#     xlim,
#     ylim,
#     code,
#     ...,
#     .pretty_axes = TRUE,
#     .max_width = 10,
#     .max_height = 8,
#     .max_distortion = 2,
#     .dpi = 96,
#     .envir = parent.frame()) {

#   if (!is.name(substitute(code))) {
#     code <- substitute(code)
#   }

#   if (.pretty_axes) {
#     px <- pretty(extendrange(xlim), bounds = TRUE)
#     py <- pretty(extendrange(ylim), bounds = TRUE)
#     xlim <- c(px[1], px[length(px)])
#     ylim <- c(py[1], py[length(py)])
#   }

#   dx <- diff(xlim)
#   dy <- diff(ylim)

#   soln <- opt_plotbox(
#     W = .max_width,
#     H = .max_height,
#     diff(xlim),
#     diff(ylim),
#     distortion_ratio = .max_distortion
#   )

#   log_info("width: {soln$w}; height: {soln$h}; distortion ratio: {soln$drat}")

#   pc_code <- substitute({
#     plot(xlim, ylim, type = "n", xlim = xlim, ylim = ylim)
#     code
#   })

#   plot_context(
#     pc_code,
#     xaxs = "i",
#     yaxs = "i",
#     ...,
#     .width = soln$w,
#     .height = soln$h,
#     .dpi = .dpi,
#     .envir = .envir
#   )
# }