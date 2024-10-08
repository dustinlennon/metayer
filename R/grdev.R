
# supported graphics devices  ---------------------------------------------------

grdev_factory <- function(grdev_name, ext = NULL, close_on_finish = TRUE) {
  list(
    name = grdev_name,
    cmd = utils::getFromNamespace(grdev_name, "grDevices"),
    ext = ext %||% glue(".{grdev_name}"),
    close_on_finish = close_on_finish
  )
}

png_dev <- grdev_factory("png")
pdf_dev <- grdev_factory("pdf")
svg_dev <- grdev_factory("svg")
jpeg_dev <- grdev_factory("jpeg", ext = ".jpg")
x11_dev <- grdev_factory("x11", close_on_finish = FALSE)

# grdev functions  ------------------------------------------------------------

#' get grdev information
#' 
#' @keywords internal
#' @param grdev_name the name of the graphics object
grdev_get <- function(grdev_name) {
  get(sprintf("%s_dev", grdev_name))
}

#' resolve file and filename
#' 
#' `file`` and `filename` are grdev specific.  They should both be available downstream and 
#' they should be in agreement.
#' 
#' @keywords internal
#' @inheritParams grdev_adapt
grdev_resolve <- function(grdev, argset) {
  # ensure that 'file' and 'filename' are equivalent
  file_name <- argset$file %||% argset$filename %||% tempfile()
  if (is_null(argset$file)) {
    argset$file <- file_name
  }
  if (is_null(argset$filename)) {
    argset$filename <- file_name
  }
  if (argset$file != argset$filename) {
    cli_abort("mismatched parameters: 'file' and 'filename' are not reconcilable.")
  }

  argset$file <- argset$filename <- fs::path_ext_set(file_name, grdev$ext)

  argset
}

#' subset device args
#' 
#' Update the default graphics device args with argset.  If argset provides
#' spurious parameters, they will be ignored.
#' 
#' @keywords internal
#' @param grdev a graphics object acquired from grdev_get
#' @param argset a passed argument set
grdev_adapt <- function(grdev, argset) {
  default_args <- as.list(formals(grdev$cmd))
  default_args$... <- NULL
  
  additional_args <- argset[names(argset) %in% names(default_args)]
  update_list(default_args, additional_args)
}

.with_grdev <- function(
    grdev_name,
    .expr,
    dev_args = list(),
    par_opts = list(),
    .envir = parent.frame()) {

  grdev <- grdev_get(grdev_name)

  dev_args <- grdev_resolve(grdev, dev_args)
  file_name <- dev_args$file

  dev_args <- grdev_adapt(grdev, dev_args)

  # debug dev_args
  log_debug(".with_grdev: grdev_name: {grdev$name}")
  for (k in names(dev_args)) {
    v <- capture.output(str(dev_args[[k]]))
    log_debug(".with_grdev: dev_args: {k} {v}")
  }

  do.call(
    grdev$cmd,
    dev_args
  )

  withr::with_par(
    par_opts,
    {
      tryCatch(
        eval(.expr, envir = .envir),
        finally = {
          dev.flush()
          if (grdev$close_on_finish)
            dev.off()
        }
      )        
    }
  )    

  img <- if (fs::file_exists(file_name)) {
    xfun::read_bin(file_name)
  } else {
    NULL
  }

  invisible(img)
}


#' execute code in a graphics context
#' 
#' Easily execute a code block in a specified graphics context.  
#' @param grdev_name the name of the graphics device, e.g. "svg" or "png"
#' @param code client code block
#' @param width width
#' @param height height
#' @param res resolution
#' @param units units of width and height
#' @param ... passed to dev_args
#' @param par_opts graphical parameters, rather than passed through ... in plot
#' @param .envir the environment in which to evaluate the code
#' @export
with_grdev <- function(
    grdev_name,
    code,
    width = getOption("mty.fig.width", 7),
    height = getOption("mty.fig.height", 7),
    res = getOption("mty.fig.res", 96),
    units = getOption("mty.fig.units", "in"),
    ...,
    par_opts = list(),
    .envir = parent.frame()) {

  .expr <- substitute(code)

  dev_args <- list(
    width = width,
    height = height,
    res = res,
    units = units,
    ...
  )

  .with_grdev(
    grdev_name,
    .expr,
    dev_args = dev_args,
    par_opts = par_opts,
    .envir = .envir
  )
}
