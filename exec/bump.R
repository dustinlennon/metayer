#! /usr/bin/env -S Rscript --vanilla  # nolint


# main script ---------------------------------------------

base_dir <- here::here()
repo <- git2r::repository(base_dir)
s <- summary(repo)

is_clean <- length(s$staged) == 0 &&
  length(s$unstaged) == 0 &&
  length(s$untracked) == 0

if (!is_clean) {
  rlang::abort("repo is not clean")
}



usethis::use_version(push = FALSE)

ver <- desc::desc()$get_version()

readr::write_lines(ver, ".version")