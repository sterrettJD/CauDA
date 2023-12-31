% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{CauDAbox-class}
\alias{CauDAbox-class}
\title{An S4 class to contain all input data as well as a dictionary to link them.}
\description{
CauDAbox is the main container that will hold your input data thoughout the \code{CauDA} pipeline.
}
\section{Slots}{

\describe{
\item{\code{metadata}}{A matrix of feature metadata. Rows are samples and columns are metadata information.}

\item{\code{tableX}}{A matrix of feature counts. Rows are samples and columns are features.}

\item{\code{dictionary}}{A asymmetric, square edge matrix (i rows by j columns) that represents causal effects of feature i on feature j}
}}

