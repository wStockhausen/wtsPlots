#'
#' @title Plot size/age comps in a dataframe as circles     
#' 
#' @description Function to plot size/age comps in a dataframe as circles.       
#'
#' @param dfr   : dataframe with columns xcol, ycol, zcols 
#' @param xcol  : dfr column name with x-axis positions
#' @param ycol  : dfr column name with y-axis positions
#' @param zcols : dfr column names with values to plot as circles
#' @param hasCutPts : flag indicating whether y-axis positions are cut points 
#' @param transform : flag (T/F) to scale circle size by area rather than diameter
#' @param overplot : flag to draw on an existing plot
#' @param maxRadius= max radius for circles (axis?)
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
#' @import graphics
#' 
#' @export
#' 
plotCompsAsCircles.dataframe<-function(dfr,
                                       zcols='z',
                                       xcol='x',
                                       ycol='y',
                                       hasCutPts=TRUE,  #are y-axis positions given as cutpts?
                                       transform=TRUE,
                                       overplot=FALSE,
                                       maxRadius=0.5,
                                       scale=NULL,
                                       xjit=0.0,
                                       fg="black",
                                       bg="blue",
                                       lty="solid",
                                       transparency=0.5,
                                       plotCohorts=FALSE,
                                       units="data units",
                                       main=NA,
                                       subtitle="",
                                       xlims=NULL,
                                       ylims=NULL,
                                       xlab='years',
                                       ylab='size',
                                       xaxt=NULL,
                                       yaxt=TRUE,
                                       debug=FALSE){
  for (iz in 1:length(zcols)){
    zcol<-zcols[iz]
    #reshape (transpose) the dataframe w/ size as 'wide' dimension
    res<-reshape(dfr[,c(xcol,ycol,zcol)],direction='wide',idvar=c(xcol),timevar=ycol);
    
    #set up size comps matrix to plot
    years<-res[,xcol];
    nc<-ncol(res);
    cutpts.names<-names(res)[2:nc];
    cutpts<-as.numeric(sub(paste(zcol,'.',sep=''),'',cutpts.names));
    if (hasCutPts){
      ncp<-length(cutpts);
      bins<-0.5*(cutpts+c(cutpts[2:(nc-1)],cutpts[ncp]+(cutpts[ncp]-cutpts[ncp-1])));#bin midpoints
    } else {
      bins<-cutpts;
    }
    
    trz<-res[,2:nc]; rownames(trz)<-as.character(years); colnames(trz)<-as.character(bins);
    
    #make the plot
    scl<-plotCompsAsCircles(z=t(trz),
                            x=years,
                            y=bins,                             
                            transform=transform,
                            overplot=overplot|(iz>1),
                            maxRadius=maxRadius,
                            scale=scale,
                            xjit=xjit,
                            fg=fg[iz],
                            bg=bg[iz],
                            lty=lty[iz],
                            transparency=transparency,
                            plotCohorts=plotCohorts,
                            units="data units",
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
