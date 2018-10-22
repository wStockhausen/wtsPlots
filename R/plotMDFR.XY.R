#'
#'@title Plot melted dataframe using an xy-plot format.
#'
#'@description Function to plot a melted dataframe using an xy plot format.
#'
#'@param mdfr - melted dataframe
#'@param x - column name for x-axis values
#'@param value.var - column name for values to aggregate (value.var in cast)/plot on y-axis
#'@param agg.formula - aggregation formula (left-hand side of cast formula)
#'@param agg.function - aggregation function (fun.aggregate in cast, default=wtsUtilities::Sum)
#'@param colour - column name to which colour aesthetic is mapped
#'@param fill - column nameto which fill aesthetic is mapped
#'@param linetype - column name to which linetype aesthetic is mapped
#'@param shape -  column name to which shape aesthetic is mapped
#'@param dodge - width to dodge overlapping points/lines
#'@param facet_grid - faceting formula for facet_grid'ing
#'@param facet_wrap - faceting formula for facet_wrap'ing
#'@param nrow - number of rows when using facet_wrap
#'@param ncol - number of columns when using facet_wrap
#'@param dir - fastest direction ('h','v') for faceting
#'@param scales - scaling flag when faceting
#'@param xlim - x axis limits
#'@param ylim - y axis limits
#'@param xlab - x axis label
#'@param ylab - y axis label
#'@param units - combined with y axis label
#'@param lnscale - flag to plot on ln-scale
#'@param title - plot title
#'@param guideTitleColour - title for colour guide
#'@param guideTitleFill - title for fill guide
#'@param guideTitleLineType - title for linetype guide
#'@param guideTitleShape - title for shape guide
#'@param plotPoints - flag to plot points
#'@param plotLines - flag to plot lines
#'@param plotABline - flag to plot a straight line
#'@param abline - list w/ components intercept, slope, colour, size, linetype, alpha describing line to plots
#'@param ggtheme - ggplot2 theme for plot (default=\code{ggplot2::theme_gray()})
#'@param showPlot - flag to show plot immediately
#'
#'@return ggplot2 object
#'
#'@details uses \code{reshape2} package.
#'
#'@import ggplot2
#'
#'@export
#'
plotMDFR.XY<-function(mdfr,
                       x=NULL,
                       value.var='val',
                       agg.formula=NULL,
                       agg.function=wtsUtilities::Sum,
                       colour=NULL,
                       fill=NULL,
                       linetype=NULL,
                       shape=NULL,
                       dodge=0.0,
                       facet_grid=NULL,
                       facet_wrap=NULL,
                       nrow=NULL,
                       ncol=NULL,
                       dir='h',
                       scales="fixed",
                       xlim=NULL,
                       ylim=NULL,
                       xlab="",
                       ylab="",
                       units="",
                       lnscale=FALSE,
                       title="",
                       guideTitleColour=NULL,
                       guideTitleFill=NULL,
                       guideTitleLineType=NULL,
                       guideTitleShape=NULL,
                       plotPoints=TRUE,
                       plotLines=TRUE,
                       plotABline=FALSE,
                       abline=list(intercept=0,slope=1,colour='black',linetype=3,size=1,alpha=0.8),
                       ggtheme=ggplot2::theme_gray(),
                       showPlot=FALSE
                       ){
    #cast melted dataframe
    if (!is.null(agg.formula)){
        #aggregate using formula
        form<-paste(agg.formula,".",sep="~")
        mdfr<-reshape2::dcast(mdfr,form,fun.aggregate=agg.function,value.var=value.var);
    } else {
        #rename value.var column to '.'
        nms<-colnames(mdfr);
        nms[nms==value.var]<-'.';
        colnames(mdfr)<-nms;
    }
    
    #setp up labels
    ylb<-ylab;
    if (units!='') ylb<-paste(ylab," (",units,")",sep='')
    if (lnscale) {
        mdfr[['.']]<-log(mdfr[['.']]);
        ylb<-paste(ylab," (ln-scale)",sep='')
        if (units!='') ylb<-paste(ylab," (",units,", ln-scale)",sep='')
    }
    
    #plot resulting dataframe
    pd<-position_dodge(width=dodge);
    p <- ggplot(aes_string(x=x,y='.',colour=colour,fill=fill,linetype=linetype,shape=shape),data=mdfr);
    p <- p + ggtheme;
    if (plotPoints) p <- p + geom_point(position=pd);
    if (plotLines)  p <- p + geom_line(position=pd);
    if (plotABline){
        p <- p + geom_abline(intercept=abline$intercept,slope=abline$slope,
                             colour=abline$colour,linetype=abline$linetype,
                             size=abline$size,alpha=abline$alpha)
    }
    if (!is.null(xlim))     p <- p + coord_cartesian(xlim=xlim);
    if (!is.null(ylim))     p <- p + coord_cartesian(ylim=ylim);
    if (!is.null(xlab))     p <- p + xlab(xlab);
    if (!is.null(ylb))      p <- p + ylab(ylb);
    if (!is.null(title))    p <- p + ggtitle(title);
    if (!is.null(facet_grid)) {
      if (packageVersion("ggplot2")<="2.2.1") {p <- p + facet_grid(as.formula(facet_grid),scales=scales);}
      if (packageVersion("ggplot2")>="3.0.0") {p <- p + facet_grid(rows=as.formula(facet_grid),scales=scales);}
    }
    if (!is.null(facet_wrap)) p <- p + facet_wrap(facet_wrap,nrow=nrow,ncol=ncol,dir=dir,scales=scales)
    if (!is.null(guideTitleColour))   p <- p + guides(colour  =guide_legend(title=guideTitleColour,override.aes=list(alpha=1.0,size=6,order=1)));
    if (!is.null(guideTitleFill))     p <- p + guides(fill    =guide_legend(title=guideTitleFill));
    if (!is.null(guideTitleLineType)) p <- p + guides(linetype=guide_legend(title=guideTitleLineType));
    if (!is.null(guideTitleShape))    p <- p + guides(shape   =guide_legend(title=guideTitleShape));
    if (showPlot) print(p);
    return(p)
}
