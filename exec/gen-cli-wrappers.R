#!/usr/bin/env -S Rscript --vanilla  # nolint

library(magrittr)

ops <- list(
  info = list(),
  warn = list(
    "cli_alert_danger",
    "cli_alert_warning",
    "cli_warn"
  ),
  error = list(
    "cli_abort"
  ),
  omit = list(
    "cli_fmt",
    "cli_list_themes",
    "cli_output_connection",
    "cli_progress_builtin_handlers",
    "cli_progress_cleanup",
    "cli_progress_num",
    "cli_progress_styles",
    "cli_sitrep",
    "cli_tick_reset"
  )
)

cli_exports <- getNamespaceExports("cli")
cli_names <- grep("^cli_", cli_exports, value = TRUE) %>%  
  setdiff(ops$omit) %>%
  sort() 

get_level <- function(ops, name) {
  # check name against each list in ops; first category (info) is the defacto default
  key <- names(which.max(sapply(ops, function(l) name %in% l)))
  switch(
    key,
    info = "logger::INFO",
    warn = "logger::WARN",
    error = "logger::ERROR"
  )
}

cli_levels <- cli_names %>%
  rlang::set_names() %>%
  purrr::imap(
    \(n, i) list(name = i, level = get_level(ops, n))
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
  
