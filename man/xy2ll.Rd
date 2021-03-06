% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proj.R
\name{xy2ll}
\alias{xy2ll}
\title{Transform [x,y] values to lon,lat coordinates}
\usage{
xy2ll(x, y = NULL, reflon = -71.03, reflat = 42.35, tol = 1e-04, n = 1000)
}
\arguments{
\item{x}{sf POINT object or a vector of x coordinates in meters relative to the reflon}

\item{y}{numeric vector of y coordinates, ignored if x is of type POINT}

\item{reflon}{numeric, the reference longitude in decimal degrees}

\item{reflat}{numeric, the reference latitude in decimal degrees}

\item{tol}{numeric, the tolorence for defining convergence}

\item{n}{numeric, the maximum number of iterations permitted when seeking convergence}
}
\value{
a tibble with lon and lat, some possibly with NaN values
}
\description{
Converted from Ernie True's xy2ll.m matlab code
}
