% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proj.R
\name{ll2xy}
\alias{ll2xy}
\title{Project [lon,lat] values to rectangular [x,y] coordinates}
\usage{
ll2xy(x, y = NULL, reflon = -71.03, reflat = 42.35)
}
\arguments{
\item{x}{sf POINT object or a vector of longitudes in decimal degrees}

\item{y}{numeric vector of latitudes, ignored if x is of type POINT}

\item{reflon}{numeric, the reference longitude in decimal degrees}

\item{reflat}{numeric, the reference latitude in decimal degrees}
}
\value{
a tibble with x and y
}
\description{
Converted from Ernie True's ll2xy.m matlab code
}
