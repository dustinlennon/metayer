# test_that("cli_inform doesn't call cli.default_handler", {

#   test_case <- function(cli_op) {
#     withr::with_options(
#       list(
#         cli.default_handler = function(msg) cat("handled")
#       ),
#       cli_op("foo")
#     )
#   }

#   expect_output(
#     test_case(cli::cli_alert),
#     "handled"
#   )

# })


# test_that("cli_inform can be wrapped", {
#   my_inform <- function(message, ..., .envir = parent.frame()) {
#     outer_cnd <- catch_cnd(
#       rlang::inform(cli::format_message(message, .envir = .envir), ...)
#     )

#     cnd <- catch_cnd(
#       cli::cli_verbatim(
#         format(outer_cnd)
#       )
#     )

#     handler <- getOption("cli.default_handler", cli:::cli_server_default)
#     handler(cnd)

#     cnd_signal(outer_cnd)
#     invisible(NULL)
#   }

#   test_case <- function(cli_op) {
#     withr::with_options(
#       list(
#         cli.default_handler = function(msg) cat("handled")
#       ),
#       cli_op("foo")
#     )
#   }

#   expect_output(
#     test_case(my_inform),
#     "handled"
#   )

#   expect_message(
#     test_case(my_inform),
#     "foo"
#   )

# })