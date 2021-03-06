% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxBaselineFunctions.R
\name{DefineTemporalCategories}
\alias{DefineTemporalCategories}
\title{Define Temporal Categories}
\usage{
DefineTemporalCategories(
  processData,
  temporalCategory = c("Quarter", "Month", "Custom"),
  customPeriods = NULL,
  years = NULL,
  encoding = "UTF-8",
  useProcessData = F
)
}
\arguments{
\item{processData}{data.table() as returned from this function}

\item{temporalCategory}{character(), defaults to 'Quarter', type of temporal category: 'Quarter', 'Month' or 'Custom'}

\item{customPeriods}{character(), provided if temporalCategory is 'Custom', vector of strings formatted as DD-MM, giving the start date of each temporal category.}

\item{years}{integer() vector, optional, provide if defintion should be non-seasonal.}

\item{encoding}{encoding of resource file}

\item{useProcessData}{logical() Bypasses execution of function, and simply returns argument 'processData'}
}
\value{
Temporal Categories, see: \code{\link[RstoxFDA]{TemporalDefinition}}.
}
\description{
StoX function
 Define temporal categories for grouping data based on date.
}
\details{
Not providing years, has the effect of making the defintion seasonal, independent of year,
 so that e.g. Q1 in 2015 is considered the same category as Q1 in 2016
}
