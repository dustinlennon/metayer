
cliops <- list(
  info = list(),
  warn = list(
    "cli_alert_warning",
    "cli_warn"
  ),
  error = list(
    "cli_abort",
    "cli_alert_danger"
  ),
  omit = list(
    "cli_fmt",
    "cli_format",
    "cli_format_method",
    "cli_list_themes",
    "cli_output_connection",
    "cli_par",
    "cli_process_done",
    "cli_process_failed",
    "cli_process_start",
    "cli_progress_along",
    "cli_progress_bar",
    "cli_progress_builtin_handlers",
    "cli_progress_cleanup",
    "cli_progress_demo",
    "cli_progress_done",   
    "cli_progress_message",
    "cli_progress_num",
    "cli_progress_output", 
    "cli_progress_styles",
    "cli_progress_step",
    "cli_progress_update",
    "cli_sitrep",
    "cli_status",
    "cli_status_clear",
    "cli_status_update",
    "cli_tick_reset"
  )
)

get_level <- function(ops, name) {
  # check name against each list in cliops; first category (info) is the defacto default
  key <- names(which.max(sapply(ops, function(l) name %in% l)))
  switch(
    key,
    info = "logger::INFO",
    warn = "logger::WARN",
    error = "logger::ERROR"
  )
}

#' build the AUTO-cli-wrappers.R file
#' 
#' @keywords internal
gen_cli_wrappers <- function() {

  cli_exports <- getNamespaceExports("cli")
  cli_names <- grep("^cli_", cli_exports, value = TRUE) %>%  
    setdiff(cliops$omit) %>%
    sort() 

  cli_levels <- cli_names %>%
    rlang::set_names() %>%
    purrr::imap(
      \(n, i) list(name = i, level = get_level(cliops, n))
    )
  names(cli_levels) <- NULL

  # render the template
  whisker::whisker.render(
    xfun::read_utf8(
      here::here("./templates/cli_wrappers.tmpl")
    ), 
    data = list(
      ops = cli_levels
    )
  ) %>%
    xfun::write_utf8(
      here::here("./R/AUTO-cli-wrappers.R")    
    )
}