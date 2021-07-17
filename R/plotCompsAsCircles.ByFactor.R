#'
#' @title Plot size/age comps as circles by levels of a factor in a dataframe 
#'
#' @description Function to plot size/age comps as circles by levels of a factor in a dataframe.
#'
#' @param dfr   : dataframe with columns xcol, ycol, zcols 
#' @param factor.name : 
#' @param factor.levels : 
#' @param zcol  : dfr column name with values to plot as circles
#' @param xcol  : dfr column name with x-axis positions
#' @param ycol  : dfr column name with y-axis positions
#' @param transform : flag (T/F) to scale circle size by area rather than diameter
#' @param overplot : flag to draw on an existing plot
#' @param maxRadius : max radius for circles (axis?)
#' @param scale : overall scale factor
#' @param xjit : jitter applied to x-axis positions
#' @param fg : foreground color for circles
#' @param bg : background color for circles
#' @param lty : line type for circles
#' @param transparency : alpha (0-1) transparency for circles
#' @param plotCohorts : flag (T/F) to plot cohorts
#' @param units : text to display as units
#' @param main : plot title
#' @param subtitle : plot subtitle
#' @param xlims : x-axis limits
#' @param ylims : y-axis limits
#' @param xlab : x-axis label
#' @param ylab : y-axis label
#' @param xaxt : 
#' @param yaxt : flag (T/F)
#' @param debug : flag to print debugging info
#' 
#' @details calls plotCompsAsCircles() to make plot.
#' 
#' @return value used to scale the circles.
#' 
#'@importFrom wtsUtilities getFactorLevels
#'
#'@export
#'
plotCompsAsCircles.ByFactor<-function(dfr=NULL,
                                      factor.name=NULL,
                                      factor.levels=NULL,
                                      zcol='z',
                                      xcol='year',
                                      ycol='size',
                                      transform=TRUE,
                                      overplot=FALSE,
                                      maxRadius=0.5,
                                      scale=NULL,
                                      xjit=0,
                                      fg=c("black","grey"),
                                      bg=c("blue","green"),
                                      lty=c("solid","dotted"),
                                      transparency=0.5,
                                      plotCohorts=FALSE,
                                      units="data units",
                                      main=NA,
                                      subtitle="",
                                      xlims=NULL,
                                      ylims=NULL,
                                      xlab="",
                                      ylab="",
                                      xaxt=TRUE,
                                      yaxt=TRUE,
                                      debug=FALSE){
  #get factor levels
  if (is.null(factor.levels)){
    factor.levels<-getFactorLevels(dfr,factor.name);
    factor.levels<-factor.levels[[factor.name]];
    cat("factor levels:",factor.levels,"\n")
  }
  
  #plot factor levels
  for (ir in 1:length(factor.levels)){
    #filter the input data frame for each factor level
    cat("Plotting factor level",factor.levels[ir],"\n")
    dfrp<-dfr[dfr[[factor.name]]==factor.levels[ir],c(xcol,ycol,zcol)];
    
    scl<-plotCompsAsCircles.dataframe(dfrp,   
                                      xcol=xcol,
                                      ycol=ycol,
                                      zcols=zcol,
                                      transform=transform,
                                      overplot=overplot|(ir>1),
                                      maxRadius=maxRadius,
                                      scale=scale,
                                      xjit=xjit*(ir-1),
                                      fg=fg[ir],
                                      bg=bg[ir],
                                      lty=lty[ir],
                                      transparency=transparency,
                                      plotCohorts=FALSE,
                                      units=units,
                                      main=main,
                                      subtitle=subtitle,
                                      xlims=xlims,
                                      ylims=ylims,
                                      xlab=xlab,
                                      ylab=ylab,
                                      xaxt=xaxt,
                                      yaxt=yaxt);
    if(is.null(scale)){scale<-scl;}
  }
  
  return(scl);
}
