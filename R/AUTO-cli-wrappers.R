# This file is dynamically generated using the ./exec/generate_cli_wrappers.R script.  DO NOT EDIT!

#' @include wrapper.R wrapper-cli.R
NULL

#' wrapped cli functions
#' 
#' These functions are wrapped versions of those in the [cli](https://cli.r-lib.org/index.html) package.
#' This allows us to inject logging metadata into the usual code flow.


#' @rdname wrapped_cli
#' @export
cli_abort <- wrapped_factory("cli::cli_abort", wrapper_cli, level = logger::ERROR)

#' @rdname wrapped_cli
#' @export
cli_alert <- wrapped_factory("cli::cli_alert", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_alert_danger <- wrapped_factory("cli::cli_alert_danger", wrapper_cli, level = logger::WARN)

#' @rdname wrapped_cli
#' @export
cli_alert_info <- wrapped_factory("cli::cli_alert_info", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_alert_success <- wrapped_factory("cli::cli_alert_success", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_alert_warning <- wrapped_factory("cli::cli_alert_warning", wrapper_cli, level = logger::WARN)

#' @rdname wrapped_cli
#' @export
cli_blockquote <- wrapped_factory("cli::cli_blockquote", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_bullets <- wrapped_factory("cli::cli_bullets", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_bullets_raw <- wrapped_factory("cli::cli_bullets_raw", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_code <- wrapped_factory("cli::cli_code", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_div <- wrapped_factory("cli::cli_div", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_dl <- wrapped_factory("cli::cli_dl", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_end <- wrapped_factory("cli::cli_end", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_format <- wrapped_factory("cli::cli_format", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_format_method <- wrapped_factory("cli::cli_format_method", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_h1 <- wrapped_factory("cli::cli_h1", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_h2 <- wrapped_factory("cli::cli_h2", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_h3 <- wrapped_factory("cli::cli_h3", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_inform <- wrapped_factory("cli::cli_inform", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_li <- wrapped_factory("cli::cli_li", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_ol <- wrapped_factory("cli::cli_ol", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_par <- wrapped_factory("cli::cli_par", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_process_done <- wrapped_factory("cli::cli_process_done", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_process_failed <- wrapped_factory("cli::cli_process_failed", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_process_start <- wrapped_factory("cli::cli_process_start", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_progress_along <- wrapped_factory("cli::cli_progress_along", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_progress_bar <- wrapped_factory("cli::cli_progress_bar", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_progress_demo <- wrapped_factory("cli::cli_progress_demo", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_progress_done <- wrapped_factory("cli::cli_progress_done", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_progress_message <- wrapped_factory("cli::cli_progress_message", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_progress_output <- wrapped_factory("cli::cli_progress_output", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_progress_step <- wrapped_factory("cli::cli_progress_step", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_progress_update <- wrapped_factory("cli::cli_progress_update", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_rule <- wrapped_factory("cli::cli_rule", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_status <- wrapped_factory("cli::cli_status", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_status_clear <- wrapped_factory("cli::cli_status_clear", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_status_update <- wrapped_factory("cli::cli_status_update", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_text <- wrapped_factory("cli::cli_text", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_ul <- wrapped_factory("cli::cli_ul", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_vec <- wrapped_factory("cli::cli_vec", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_verbatim <- wrapped_factory("cli::cli_verbatim", wrapper_cli, level = logger::INFO)

#' @rdname wrapped_cli
#' @export
cli_warn <- wrapped_factory("cli::cli_warn", wrapper_cli, level = logger::WARN)
