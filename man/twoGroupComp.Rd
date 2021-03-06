% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploysPipline.R
\name{twoGroupComp}
\alias{twoGroupComp}
\title{This is a wrapper function to streamline the analysis pipeline}
\usage{
twoGroupComp(group1, group2, category, subcat = NULL, or = TRUE,
  conf = FALSE, tests = "goodness")
}
\arguments{
\item{group1}{A string of excel filename}

\item{group2}{A string of excel filename}

\item{category}{A string of comparison catelog, such as "Diagonosis"}

\item{subcat}{A string of comparison sub-catelog, such as "Heart failure"}

\item{or}{A boolean indicating if odd ratio will be computed, default is false}

\item{conf}{A boolean indicating if confidence inteval will be computed, defalut is false due to 
expensive computing.}

\item{tests}{Type of statistic test, only "goodness" and "contigency" available}
}
\value{
A dataframe of two group comparison results under "goodness" test. Otherwise a p-value will
  be given
}
\description{
This is a wrapper function to streamline the analysis pipeline
}
