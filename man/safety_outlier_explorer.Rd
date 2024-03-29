% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safety_outlier_explorer.R
\name{safety_outlier_explorer}
\alias{safety_outlier_explorer}
\title{Safety Outlier Explorer}
\usage{
safety_outlier_explorer(data, settings)
}
\arguments{
\item{data}{labs data structured as one record per person per visit per measurement. See details for column requirements.}

\item{settings}{named list of settings with the parameters specified below.}
}
\value{
returns a chart object
}
\description{
Safety Outlier Explorer
}
\details{
The settings object provides details the columns in the data set.

\itemize{
\item{"id_col"}{ID column}
\item{"value_col"}{Value column}
\item{"measure_col"}{Measure column}
\item{"measure_values"}{Measure values}
\item{"studyday_col"}{Study Day (numeric)}
}
}
\examples{

settings <- list(
    id_col = "USUBJID",
    measure_col = "LBTEST",
    measure_values = c("Albumin", "Bilirubin", "Chloride"),
    studyday_col = "VISITDY",
    value_col = "LBORRES"
)
safety_outlier_explorer(safetyData::sdtm_lb, settings)
}
