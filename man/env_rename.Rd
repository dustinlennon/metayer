% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-env.R
\name{env_rename}
\alias{env_rename}
\title{rename an environment}
\usage{
env_rename(e, name, envir = parent.frame())
}
\arguments{
\item{e}{the environment}

\item{name}{the new name, to be passed to str_glue}

\item{envir}{the environment to pass to str_glue}
}
\value{
the environment with the 'name' attribute set
}
\description{
rename an environment
}
