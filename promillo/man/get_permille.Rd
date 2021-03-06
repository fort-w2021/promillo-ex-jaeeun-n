% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tell_me_how_drunk.R
\name{get_permille}
\alias{get_permille}
\title{calculate the blood alcohol per mille}
\usage{
get_permille(alcohol_drunk, bodywater, drinking_time)
}
\arguments{
\item{alcohol_drunk}{single numeric value of amount of alcohol consumed; preferably output of function 'get_alcohol'}

\item{bodywater}{single numeric value of bodywater; preferably output of function 'get_bodywater'}

\item{drinking_time}{vector of two POSIXct values indicating when person started and ended drinking}
}
\value{
single numeric value
}
\description{
The formula is based on a combination of Whatson's and Widmark's formula.
}
