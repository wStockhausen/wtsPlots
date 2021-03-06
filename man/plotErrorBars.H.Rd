% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotErrorBars.H.R
\name{plotErrorBars.H}
\alias{plotErrorBars.H}
\title{Add horizontal error bars to current plot}
\usage{
plotErrorBars.H(
  x,
  y,
  upper = NULL,
  lower = NULL,
  sigma = NULL,
  cv = NULL,
  CI = 0.95,
  lognormal = FALSE,
  width = 1,
  ...
)
}
\arguments{
\item{x}{- vector of x coordinates or matrix w/ columns 1 & 2 taken as x,y}

\item{y}{- vector of y coordinates (or NULL, if x is a matrix)}

\item{upper}{- upper CI for y}

\item{lower}{- lower CI for y}

\item{sigma}{- standard deviations for y}

\item{cv}{- cvs for y, instead of sigma}

\item{CI}{- upper confidence interval (if sigma or cv specified)}

\item{lognormal}{- flag (T/F). if true, assumed distribution is lognormal}

\item{width}{- relative width of error bars}

\item{...}{- graphics parameters for segments (e.g., col, lty, lwd)}
}
\description{
Adds horizontal error bars to current plot.
}
\details{
Adds horizontal errors bars to current plot.
}
