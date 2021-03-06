% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateCosineDelta.R
\name{calculateCosineDelta}
\alias{calculateCosineDelta}
\title{Calculate Cosine Delta}
\usage{
calculateCosineDelta(input_matrix, already_zscored = TRUE)
}
\arguments{
\item{input_matrix}{zscores in a matrix; col = features, row = individuals}

\item{already_zscores}{Is the data already scaled & centered? Default is TRUE}
}
\description{
This function creates a distance table from zscores, using Cosine Delta.
}
\examples{
calculateCosineDelta()
}
\keyword{distance}
\keyword{table}
