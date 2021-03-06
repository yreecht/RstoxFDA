% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxBaselineFunctions.R
\name{DefineAgeErrorMatrix}
\alias{DefineAgeErrorMatrix}
\title{Define Age Error Matrix}
\usage{
DefineAgeErrorMatrix(
  processData,
  resourceFilePath,
  encoding = "UTF-8",
  useProcessData = F
)
}
\arguments{
\item{processData}{data.table() as returned from this function}

\item{resourceFilePath}{path to resource file}

\item{encoding}{encoding of resource file}

\item{useProcessData}{logical() Bypasses execution of function, and simply returns argument 'processData'}
}
\value{
Age Error Matrix, see: \code{\link[RstoxFDA]{AgeErrorMatrix}}.
}
\description{
StoX function.
 Defines probabilities for misreading ages.
}
\details{
Definitions are read from a tab separated file with headers and row names in first column.
 All row and column names should be integers.
 The matrix encodes the probability of observing an age (rows), given true age (columns).
 Columns must sum to 1.
}
