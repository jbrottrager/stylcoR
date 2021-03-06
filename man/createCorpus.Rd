% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createCorpus.R
\name{createCorpus}
\alias{createCorpus}
\title{Create a Corpus}
\usage{
createCorpus(path_to_corpus, remove_whitespaces = TRUE,
  lowercase = TRUE, remove_punctuation = TRUE)
}
\arguments{
\item{path_to_corpus}{path to the corpus directory}

\item{remove_whitespaces}{determines if whitespaces should be removed,
default = TRUE}

\item{lowercase}{determines if all cases should be turned into lowercase,
default = TRUE}

\item{remove_punctuation}{determines if all punctuation characters should be
removed, default = TRUE}
}
\description{
This function creates a quanteda corpus
}
\examples{
createCorpus()
}
\keyword{corpus}
