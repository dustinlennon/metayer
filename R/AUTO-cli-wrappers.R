# This file is dynamically generated using the ./exec/generate_cli_wrappers.R script.  DO NOT EDIT!

#' @include wrapped-factory.R wrapped-with-logger.R
NULL

#' wrapped cli functions
#' 
#' These are wrapped versions of [cli](https://cli.r-lib.org/index.html) methods.  The
#' wrapper inserts a condition handler that produces log data.


#' @rdname wrapped_cli
#' @inherit cli::cli_abort
#' @export
cli_abort <- wrapped_factory("cli::cli_abort", wrapped_with_logger, level = logger::ERROR)

#' @rdname wrapped_cli
#' @inherit cli::cli_alert
#' @export
cli_alert <- wrapped_factory("cli::cli_alert", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_alert_danger
#' @export
cli_alert_danger <- wrapped_factory("cli::cli_alert_danger", wrapped_with_logger, level = logger::ERROR)

#' @rdname wrapped_cli
#' @inherit cli::cli_alert_info
#' @export
cli_alert_info <- wrapped_factory("cli::cli_alert_info", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_alert_success
#' @export
cli_alert_success <- wrapped_factory("cli::cli_alert_success", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_alert_warning
#' @export
cli_alert_warning <- wrapped_factory("cli::cli_alert_warning", wrapped_with_logger, level = logger::WARN)

#' @rdname wrapped_cli
#' @inherit cli::cli_blockquote
#' @export
cli_blockquote <- wrapped_factory("cli::cli_blockquote", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_bullets
#' @export
cli_bullets <- wrapped_factory("cli::cli_bullets", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_bullets_raw
#' @export
cli_bullets_raw <- wrapped_factory("cli::cli_bullets_raw", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_code
#' @export
cli_code <- wrapped_factory("cli::cli_code", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_div
#' @export
cli_div <- wrapped_factory("cli::cli_div", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_dl
#' @export
cli_dl <- wrapped_factory("cli::cli_dl", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_end
#' @export
cli_end <- wrapped_factory("cli::cli_end", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_h1
#' @export
cli_h1 <- wrapped_factory("cli::cli_h1", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_h2
#' @export
cli_h2 <- wrapped_factory("cli::cli_h2", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_h3
#' @export
cli_h3 <- wrapped_factory("cli::cli_h3", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_inform
#' @export
cli_inform <- wrapped_factory("cli::cli_inform", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_li
#' @export
cli_li <- wrapped_factory("cli::cli_li", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_ol
#' @export
cli_ol <- wrapped_factory("cli::cli_ol", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_progress_message
#' @export
cli_progress_message <- wrapped_factory("cli::cli_progress_message", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_progress_output
#' @export
cli_progress_output <- wrapped_factory("cli::cli_progress_output", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_progress_step
#' @export
cli_progress_step <- wrapped_factory("cli::cli_progress_step", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_rule
#' @export
cli_rule <- wrapped_factory("cli::cli_rule", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_text
#' @export
cli_text <- wrapped_factory("cli::cli_text", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_ul
#' @export
cli_ul <- wrapped_factory("cli::cli_ul", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_vec
#' @export
cli_vec <- wrapped_factory("cli::cli_vec", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_verbatim
#' @export
cli_verbatim <- wrapped_factory("cli::cli_verbatim", wrapped_with_logger, level = logger::INFO)

#' @rdname wrapped_cli
#' @inherit cli::cli_warn
#' @export
cli_warn <- wrapped_factory("cli::cli_warn", wrapped_with_logger, level = logger::WARN)

