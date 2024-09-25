#' @include pubcontext.R
NULL

splt_jupyter <- function(.expr, dev_args, par_opts, .envir = parent.frame()) {
  img_data <- .with_grdev(
    "png",
    .expr,
    dev_args = dev_args,
    par_opts = par_opts,
    .envir = .envir
  )

  pimg_as_html_data(img_data) %>%
    IRdisplay::display_html()

  invisible(NULL)
}

splt_knitr <- function(.expr, dev_args, par_opts, .envir = parent.frame()) {

  knitr_opts <- knitr::opts_knit$get()
  chunk_opts <- knitr::opts_current$get()

  chunk_id <- knitr::opts_current$get("label")
  metayer_hook <- knitr::opts_current$get("metayer_hook")
  root_dir <- knitr::opts_knit$get("root.dir") %||% "unknown"

  knitr_dev <- switch(
    knitr::opts_knit$get("rmarkdown.pandoc.to") %||% "",
    html = "png",
    pdf = "svg",
    knitr::opts_current$get("dev")
  )

  if (is.null(metayer_hook)) {
    cli_abort("sureplot: metayer_hook must be set as a chunk option: {chunk_id}")
  }

  grdev <- grdev_get(knitr_dev)

  img_data <- .with_grdev(
    grdev$name,
    .expr,
    dev_args = dev_args,
    par_opts = par_opts,
    .envir = .envir
  )

  src <- with(
    chunk_opts,
    fs::path_join(c(fig.path, label))
  ) %>% 
    fs::path_ext_set(grdev$ext)

  chunk_env <- storage_env("metayer", "chunks", chunk_id)
  chunk_env$output <- pimg_as_md_link(
    img_data,
    src
  )

  invisible(NULL)
}

splt_script <- function(.expr, dev_args, par_opts, .envir = parent.frame(), is_interactive) {
  grdev_name <- if (is_interactive) "x11" else "png"

  .with_grdev(
    grdev_name,
    .expr,
    dev_args = dev_args,
    par_opts = par_opts,
    .envir = .envir
  )
}

#' context agnostic plotting
#' 
#' Create plots that are agnostic to publishing context.
#' 
#' @param code client code
#' @param width width
#' @param height height
#' @param res resolution dpi
#' @param units units of width and height
#' @param file a file, mapped to filename, if the device expects it
#' @param ... parameters passed to device args
#' @param par_opts graphics pararameters
#' @param .envir the environment in which to evaluate the client code
#' @export
sure_plot <- function(
    code,
    width = getOption("mty.fig.width", 7),
    height = getOption("mty.fig.height", 7),
    res = getOption("mty.fig.res", 96),
    units = getOption("mty.fig.units", "in"),
    file = tempfile(),
    ...,
    par_opts = list(),
    .envir = parent.frame()) {

  .expr <- substitute(code)

  dev_args <- list(
    width = width,
    height = height,
    res = res,
    units = units,
    file = file
  )
  dev_args <- c(dev_args, list(...))

  # debug dev_args
  for (k in names(dev_args)) {
    v <- capture.output(str(dev_args[[k]]))
    log_debug("[0] sureplot: dev_args: {k} {v}")
  }
 
  pubcontext(
    jupyter_code = splt_jupyter(.expr, dev_args, par_opts, .envir = .envir),
    knitr_code = splt_knitr(.expr, dev_args, par_opts, .envir = .envir),
    interactive_code = splt_script(.expr, dev_args, par_opts, .envir = .envir, is_interactive = TRUE),
    non_interactive_code = splt_script(.expr, dev_args, par_opts, .envir = .envir, is_interactive = FALSE),
    .local_envir = .envir
  ) %>%
    invisible()
}
