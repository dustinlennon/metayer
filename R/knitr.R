knitr_set_config <- function(root.dir = NULL, dev = NULL) {
  knitr::opts_chunk$set(
    metayer_hook = TRUE,
    dev = dev %||% Sys.getenv("KNITR_CHUNK_DEV", "png")
  )  
  knitr::opts_knit$set(
    root.dir = root.dir %||% Sys.getenv("KNITR_PACKAGE_ROOT_DIR", here::here("vignettes"))
  )
  knitr::knit_hooks$set(
    metayer_hook = knitr_metayer_hook
  )
}
