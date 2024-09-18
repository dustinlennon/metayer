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
}

splt_knitr <- function(.expr, dev_args, par_opts, .envir = parent.frame()) {
  rmarkdown_pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  grdev <- if (rmarkdown_pandoc_to == "html") {    
    grdev_get("png")
  } else {
    grdev_get("pdf")
  }

  opts <- knitr::opts_current$get()
  chunk_id <- opts$metayer

  if (is.null(chunk_id)) {
    cli_abort("knitr pipeline requires the setting of a knit hook")
  }

  img_data <- .with_grdev(
    grdev$name,
    .expr,
    dev_args = dev_args,
    par_opts = par_opts,
    .envir = .envir
  )

  src <- with(
    opts,
    paste0(fig.path, label)
  ) %>% 
    fs::path_ext_set(
      grdev$ext
    )

  chunk_env <- storage_env("metayer", "chunks", chunk_id)
  chunk_env$output <- pimg_as_md_link(
    img_data,
    src
  )
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

sureplot <- function(
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
    non_interactive_code = splt_script(.expr, dev_args, par_opts, .envir = .envir, is_interactive = FALSE)
  )
}
