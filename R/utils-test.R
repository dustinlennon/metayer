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

  withr::deferred_run()

  if (testthat::is_testing() == FALSE) {
    devtools::load_all()
  }

  logger_reset()
  storage_purge()

  rm.all(
    exclusions = exclusions
  )

  if (testthat::is_testing()) {
    withr::defer_parent(
      storage_purge()
    )
  }
}

#' Capture messages in a bytestream
#' 
#' @param code client code to execute
#' @param nbytes the size of the bytearray
#' @param strip_newline if TRUE, remove a single trailing newline
#' @returns the captured output
#' @export
with_message_buf <- function(
    code,
    nbytes = 1000,
    strip_newline = TRUE) {

  con <- rawConnection(raw(nbytes), open = "wb")
  withr::defer(close(con))

  withr::with_message_sink(
    con,
    code
  )

  v <- rawToChar(rawConnectionValue(con))
  if (strip_newline) {
    gsub("\n$", "", v)
  } else {
    v
  }
}
