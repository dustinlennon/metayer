% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-publish.R
\name{decode_knitr}
\alias{decode_knitr}
\title{decode the start of an encoded knitr chunk}
\usage{
decode_knitr(enc, dict)
}
\arguments{
\item{dict}{an environment to use as a dictionary}
}
\description{
N.B., this is a convenient place to inject into the knitr chunk header, e.g.,
metayer_hook
}
\keyword{internal}
