% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggMarginal_Hist2D.R
\name{ggMarginal_Hist2D}
\alias{ggMarginal_Hist2D}
\title{Add marginal histograms to ggplot2 2d histogram plots}
\usage{
ggMarginal_Hist2D(
  data,
  x,
  y,
  size = 5,
  after_stat_var = c("ncount", "count", "density", "ndensity"),
  hist_scale = c("arithmetic", "log10", "4th-root"),
  bins = 30,
  binwidths = NULL,
  xlab = NULL,
  ylab = NULL,
  xparams = list(limits = NULL, breaks = waiver(), labels = waiver(), name = waiver(),
    oob = scales::squish),
  yparams = list(limits = NULL, breaks = waiver(), labels = waiver(), name = waiver(),
    oob = scales::squish),
  fill_scale = ggplot2::scale_fill_viridis_c(option = "magma", name = ""),
  legend.position = c(0.99, 0.99),
  legend.justification = c(1, 1),
  ...,
  groupColour = FALSE,
  groupFill = FALSE,
  showIntermediates = FALSE,
  testing = FALSE
)
}
\arguments{
\item{data}{The data.frame to use for creating the plots.}

\item{x}{The name of the variable along the x axis.}

\item{y}{The name of the variable along the y axis.}

\item{size}{Integer describing the relative size of the marginal plots
compared to the main plot. A size of 5 means that the main plot is 5x wider
and 5x taller than the marginal plots.}

\item{hist_scale}{- scale for histograms; options = ("arithmetic","log10","4th-root")}

\item{bins}{- number of bins to use (scalar or 2-element vector); default=30}

\item{binwidths}{- bin widths to use (scalar or 2-element vector); default=NULL}

\item{xlab}{- x-axis title; default=NULL (i.e., use ggplot2 default)}

\item{ylab}{- y-axis title; default=NULL (i.e., use ggplot2 default)}

\item{xparams}{- list of [ggplot2::scale_x_continuous()] parameters; default = 
list(limits=NULL,breaks=waiver(),labels=waiver(),oob=scales::squish)}

\item{yparams}{- list of [ggplot2::scale_y_continuous()] parameters; default = 
list(limits=NULL,breaks=waiver(),labels=waiver(),oob=scales::squish)}

\item{fill_scale}{- 2d histogram fill scale; default=ggplot2::scale_fill_viridis_c(option="magma",name="")}

\item{legend.position}{- legend position; default=c(0.99,0.99)}

\item{legend.justification}{- legend justification; default=c(1,1)}

\item{...}{Extra parameters to pass to the marginal plots. Any parameter that
[ggplot2::geom_histogram()] accepts can be used. 
For example, \code{colour = "red"} can be used for any marginal plot type,}

\item{groupColour}{If \code{TRUE}, the colour (or outline) of the marginal
plots will be grouped according to the variable mapped to \code{colour} in the
scatter plot. The variable mapped to \code{colour} in the scatter plot must
be a character or factor variable. See examples below.}

\item{groupFill}{If \code{TRUE}, the fill of the marginal
plots will be grouped according to the variable mapped to \code{colour} in the
scatter plot. The variable mapped to \code{colour} in the scatter plot must
be a character or factor variable.}

\item{testing}{- flag (T/F) to include}

\item{stat_var}{- name of statistical variable to plot; options=("count","ncount","density","ndensity")}
}
\value{
If \code{testing} is false, a \code{ggGTbl} object that can be printed using S3 dispatch with \code{print}.
If \code{testing} is TRUE, a list with elements \cr
\itemize{
 \item{ggGTbl - an object of class \code{ggGTbl}.}
 \item{built - a list with elements p2d, px,py corresponding to ggplot2 objects "built" with [ggplot2::ggplot_build()]}
}
}
\description{
Create a ggplot2 2d histogram plot with marginal histogram plots.
}
\note{
The \code{grid}, \code{gtable}, and \code{rlang} packages are required for this
function.

This function and related code are based on the [ggExtra::ggMarginal()] 
code developed by Dean Attali ("daattali@gmail.com").
}
\examples{
library(ggplot2)
ggGTbl = ggMarginal_Hist2D(data=mtcars,x=wt,y=mpg,
                          hist_scale="arithmetic",
                          binwidths=c(0.5,7),
                          xparams=list(limits=c(0, 3),breaks=seq(0, 7,0.5),oob=scales::squish),
                          yparams=list(limits=c(0,40),breaks=seq(0,40,2),oob=scales::squish),
                          legend.position=c(0.01,0.99),
                          legend.justification=c(0,1),
                          showIntermediates=FALSE);
print(ggGTbl,newpage=TRUE);
gglst = ggMarginal_Hist2D(data=mtcars,x=wt,y=mpg,
                          hist_scale="arithmetic",
                          binwidths=c(0.5,7),
                          xparams=list(limits=c(0, 3),breaks=seq(0, 7,0.5),oob=scales::squish),
                          yparams=list(limits=c(0,40),breaks=seq(0,40,2),oob=scales::squish),
                          legend.position=c(0.01,0.99),
                          legend.justification=c(0,1),
                          showIntermediates=FALSE,
                          testing=TRUE);
print(gglst$ggGTbl,newpage=TRUE);

}
