% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QT_Central_Tendency.R
\name{QT_Central_Tendency}
\alias{QT_Central_Tendency}
\title{QT central tendency}
\usage{
QT_Central_Tendency(data, settings)
}
\arguments{
\item{data}{ECG data structured as one record per person per visit per measurement. See details for column requirements.}

\item{settings}{named list of settings with the parameters specified below.}
}
\value{
returns a chart object
}
\description{
QT central tendency
}
\details{
The settings object provides details the columns in the data set.

\itemize{
\item{"id_col"}{ID column}
\item{"value_col"}{Value column}
\item{"measure_col"}{Measure column}
\item{"measure_values"}{Measure values}
\item{"visit_col"}{Visit column}
\item{"visitn_col"}{Visit number column (numeric)}
\item{"baseline_flag_col}{Baseline flag column}
\item{"baseline_flag_values}{Baseline flag value}
}
}
