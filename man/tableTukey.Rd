% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tableTukey.R
\name{tableTukey}
\alias{tableTukey}
\title{Create a table for Tukey HSD test results}
\usage{
tableTukey(
  audioData,
  by = c(),
  measure = "duration",
  nameMeasure = c(),
  figureNumber = 1
)
}
\arguments{
\item{audioData}{A data.frame generated by the autoExtract() function.}

\item{by}{A character vector indicating the name of the factor(s).}

\item{measure}{Name of the dependent variable.}

\item{nameMeasure}{Optional string to rename the dependent variable in the output table. If no value is provided, the original variable name is displayed.}

\item{figureNumber}{Integer indicating the figure number, used to create the title for the table. Default corresponds to 1.}
}
\value{
HTML table showing Tukey HSD test results in APA formatting style.
}
\description{
Automatically generates an HTML table with the results of a Tukey HSD test.
}
\examples{
tableTukey(testAudioData, by = "Condition", measure = "duration")

}
