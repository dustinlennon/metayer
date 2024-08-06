#!/usr/bin/env -S Rscript --vanilla

#' If the branch is clean and has an upstream / remote branch:  
#'   + bump the version number, 
#'   + update the DESCRIPTION file
#'   + commit the changes to DESCRIPTION
#'   + push the commit to remote
#'   + create a version tag
#'   + push the version tag to remote


library(magrittr)
library(optparse)

#' Process command line args
process_args <- function(args) {
  parser <- OptionParser()

  # --which
  parser <- add_option(
    parser,
    c("--which"),
    default = "patch",
    action = "callback",
    type = "character",
    help = "major, minor, patch, or dev",
    callback = function(
        opt,
        opt_name,
        val = c("major", "minor", "patch", "dev"),
        parser) {
      match.arg(val)
    }
  )

  argv <- parse_args(
    parser,
    args,
    positional_arguments = TRUE
  )

  argv$options
}

# main script ---------------------------------------------

opt <- process_args(
  args = commandArgs(trailingOnly = TRUE)
)

# Assume that the top level directory is the git root.  Set the working directory
# accordingly.
base_dir <- here::here()
setwd(base_dir)

# validate that the repo is clean
is_clean <- nrow(gert::git_status()) == 0
if (!is_clean) {
  rlang::abort("HEAD is not clean")
}

# validate that HEAD has an upstream branch
info <- gert::git_info()
if (is.na(info$remote)) {
  rlang::abort("HEAD does not have an upstream branch")
}

# bump the version
ver <- usethis:::bump_version()[opt$which]
d <- desc::desc_set_version(ver)

message <- stringr::str_glue("Increment version number to {ver}")

# add/commit DESCRIPTION
gert::git_add("DESCRIPTION")
sha <- gert::git_commit(message)

# record to .version
readr::write_lines(c(ver, sha), file = ".version")

# push to remote
gert::git_push(info$remote, info$head)

# create a version tag
tag_name <- stringr::str_glue("v{ver}")
gert::git_tag_create(
  name = tag_name,
  message = message
)

# push version tag
gert::git_tag_push(tag_name)