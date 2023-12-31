% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CauDA.R
\name{caudabox_from_csvs}
\alias{caudabox_from_csvs}
\title{Generate a CauDAbox object from CSVs}
\usage{
caudabox_from_csvs(metadata, tableX, dictionary)
}
\arguments{
\item{metadata}{This contains sample metadata, where samples are rows.}

\item{tableX}{This contains the feature data (e.g., feature count table), where samples are rows}

\item{dictionary}{This is a N x N edge matrix, where N=number of features in tableX, where each entry (row i, col j) indicates if feature i has a causal effect on feature j.}
}
\value{
A CauDAbox object with the metadata, tableX, and dictionary.
}
\description{
Should be used to read in data from csv files to create a CauDAbox.
}
