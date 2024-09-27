#' A less motivational test
#'
#' @param test_name the canonical stub name, e.g., 'logger'
#' @export
test_filter <- function(test_name) {
  my_reporter <- testthat::ProgressReporter$new(show_praise = FALSE)
  devtools::test(filter = test_name, reporter = my_reporter)
}

#' Basic test sanitation
#' 
#' This:
#'   runs any deferred tasks that may be pending
#'   calls devtools::load_all if not testing
#'   resets loggers
#'   purges the storage
#'   calls rm.all with noted exclusions
#'   sets up a deferred storage purge on exit
#' 
#' @param exclusions a list of object names to exclude from rm.all
#' @param envir a local environment
#' @export
test_sanitize <- function(
    exclusions = c(),
    envir = parent.frame()) {

  Sys.setenv(R_CONFIG_ACTIVE = "testing")

  if (testthat::is_testing() == FALSE) {
    devtools::load_all()
    storage_purge()
    rm.all(
      exclusions = exclusions
    )
  }
  .onLoad(NULL, "metayer")

}

#' Create a predictable identifier sequence
#' 
#' This is probably most useful when testing, as one can set the corresponding entry
#' in the config.yml to get reproducible results.
#' 
#' @param seed a seed for the hash function
#' @export
test_mty_uuid <- function(seed = NULL) {
  seed <- seed %||% getOption("uuid.generator.seed", "undefined")
  result <- hash(seed)
  options(uuid.generator.seed = result)

  sprintf(
    "%s-%s-%s-%s-%s",
    stringr::str_sub(result, 1, 8),
    stringr::str_sub(result, 8, 12),
    stringr::str_sub(result, 12, 16),
    stringr::str_sub(result, 16, 20),
    stringr::str_sub(result, 20, 32)
  )
}
