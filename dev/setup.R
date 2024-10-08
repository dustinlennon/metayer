#!/usr/bin/env -S Rscript --vanilla  # nolint

#
# This script installs the package--e.g., foo--into the user space
# library, i.e.:
#   $HOME/.local/lib/R/library/foo
#


library(magrittr)
library(optparse)

# load the configuration
cfg <- yaml::read_yaml(
  here::here("dev/setup.yml")
)

#' Process command line args
process_args <- function(args) {
  parser <- OptionParser()

  # --reinit
  parser <- add_option(
    parser,
    c("--reinit"),
    action = "store_true",
    default = FALSE,
    help = "reinitialize"
  )

  argv <- parse_args(
    parser,
    args,
    positional_arguments = TRUE
  )

  argv$options
}

#' Add vignettes
add_vignettes <- function(vignettes) {
  for (v in vignettes) {
    usethis::use_vignette(v)
  }
}

create_symbolic_link <- function(base_dir, src, dst)  {
  src <- fs::path_join(c(base_dir, src)) %>%
    fs::path_norm()
  dst <- fs::path_join(c(base_dir, dst)) %>%
    fs::path_norm()

  fs::link_create(src, dst)
}

#' Add package dependencies
add_package_dependencies <- function(packages, import_from) {
  for (pkg in packages) {
    usethis::use_package(pkg)
  }

  for (iter in import_from) {
    do.call(usethis::use_import_from, iter)
  }
}

add_package_suggestions <- function(suggestions) {
  for (pkg in suggestions) {
    usethis::use_package(pkg, type = "Suggests")
  }
}

#' Update the description field
add_package_description <- function(description) {

  # Use the desc package to manipulate the DESCRIPTION file.
  d <- desc::desc()

  fmt_desc <- stringr::str_wrap(
    description,
    width = 100,
    indent = 0,
    exdent = 2
  )

  d$set(Description = fmt_desc)
  d$write()
}

#' Ensure that the repo is clean
#' 
#' If the repo is dirty, abort.
validate_clean_repo <- function(base_dir)  {
  is_clean <- nrow(gert::git_status()) == 0
  if (!is_clean) {
    rlang::abort("HEAD is not clean")
  }
}

#' Select an add_package implementation
#' 
#' @param pkg_type a known implementation
#' @returns an add_package implementation
add_package_factory <- function(pkg_type = c("repo", "cran")) {
  pkg_type <- match.arg(pkg_type)
  switch(pkg_type,
    repo = add_repo_package,
    {
      cli_abort(
        "unsupported pkg_type: {pkg_type}",
        .class = "value-error"
      )
    }
  )
}

#' Add a package from a git repo
#' 
#' Installs the package from the git url and updates the DESCRIPTION file
add_repo_package <- function(local_package) {
  pkg_dep <- local_package$name
  pkg_ver <- local_package$version
  pkg_typ <- local_package$type
  git_url <- local_package$url
  
  # Assume that the git tag is the semantic version prefaced by a "v"
  if (!rlang::is_installed(pkg_dep)) {
    remotes::install_git(
      git_url,
      ref = stringr::str_glue("v{pkg_ver}"),
      build_vignettes = FALSE
    )
  }

  # Use the desc package to manipulate the DESCRIPTION file.
  d <- desc::desc()

  d$set_dep(
    pkg_dep,
    pkg_typ,
    version = stringr::str_glue("== {pkg_ver}")
  )

  d$set_remotes(
    stringr::str_glue("git::{git_url}@v{pkg_ver}")
  )

  d$write()
}

#' main script ------------------------------------------------------------------------
#' 
#' Uses devtools to set up a package
#' 
#'   - create_package
#'   - use_package_doc
#'   - use_mit_license
#'   - use_testthat
#' 

opt <- process_args(
  args = commandArgs(trailingOnly = TRUE)
)

base_dir <- cfg$base_dir

desc_filename <- fs::path_join(
  c(
    base_dir,
    "DESCRIPTION"
  )
)

reinitialize <- opt$reinit ||
  !fs::dir_exists(base_dir) ||
  !fs::file_exists(desc_filename)

if (reinitialize) {
  tryCatch(
    usethis::create_package(base_dir),
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      cat(msg, "\n")
      quit("no", status = 1, runLast = FALSE)
    }
  )
}

usethis::proj_set(base_dir)

usethis::use_package_doc()
usethis::use_mit_license()
usethis::use_testthat(3)
usethis::use_news_md()
usethis::use_cran_comments()

create_symbolic_link(cfg$base_dir, "./inst/.lintr", ".lintr")
create_symbolic_link(cfg$base_dir, "./inst/.Rprofile", ".Rprofile")

add_vignettes(cfg$usethis$vignettes)
add_package_dependencies(cfg$usethis$packages, cfg$usethis$import_from)
add_package_suggestions(cfg$usethis$suggests)
add_package_description(cfg$description)

# Set up package dependencies using a local library
withr::with_libpaths(
  here::here("library"),
  {
    add_package <- add_package_factory("repo")
    for (iter in cfg$local_package) {
      add_package(iter)
    }

    # document the package
    devtools::document()
  }
)

# install the package
validate_clean_repo(cfg$base_dir)
devtools::install(
  quick = TRUE,
  upgrade = "never"
)
