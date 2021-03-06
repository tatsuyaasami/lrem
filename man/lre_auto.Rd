% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lre_auto.R
\name{lre_auto}
\alias{lre_auto}
\title{Solve a linear autonomous system with Klein method}
\usage{
lre_auto(A, E = NULL, x0)
}
\arguments{
\item{A}{dim(A[1]) = dim(A)[2], n_by_n matrix}

\item{E}{dim(E[1]) = dim(E)[2], n_by_n matrix}

\item{x0}{x0 in R, Initial value of predetermined vector}
}
\value{
Functions of predetermined vector at t on un-predetermined vector at t and predetermined vector at t+1
}
\description{
Solve a linear autonomous system with Klein method
}
