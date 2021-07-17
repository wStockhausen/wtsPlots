#'
#'@title Plot melted dataframe using a barchart/line-plot format.
#'
#'@description Function to plot a melted dataframe using a barchart/line-plot format.
#'
#'@param mdfr - melted dataframe
#'@param x - column name for x-axis values
#'@param value.var - column name for values to aggregate (value.var in cast)/plot on y-axis
#'@param agg.formula - aggregation formula (left-hand side of cast formula)
#'@param agg.function - aggregation function (fun.aggregate in cast)
#'@param ... - further arguments passed to aggregating function
#'@param colour - column name to which colour aesthetic is mapped
#'@param fill - column name to which fill aesthetic is mapped
#'@param linetype - column name to which linetype aesthetic is mapped
#'@param shape -  column name to which shape aesthetic is mapped
#'@param faceting - faceting formula
#'@param xlab - x axis label
#'@param ylab - y axis label
#'@param units - combined with y axis label
#'@param lnscale - flag to plot on ln-scale
#'@param title - plot title
#'@param guideTitleColour - title for colour guide
#'@param guideTitleFill - title for fill guide
#'@param guideTitleLineType - title for linetype guide
#'@param guideTitleShape - title for shape guide
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
plotMDFR.BarLine<-function(mdfr,
                           x=NULL,
                           value.var='val',
                           agg.formula=NULL,
                           agg.function=sum,
                           ...,
                           colour=NULL,
                           fill=NULL,
                           linetype=NULL,
                           shape=NULL,
                           faceting=NULL,
                           xlab="",
                           ylab="",
                           units="",
                           lnscale=FALSE,
                           title="",
                           guideTitleColour=NULL,
                           guideTitleFill=NULL,
                           guideTitleLineType=NULL,
                           guideTitleShape=NULL,
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
    p <- ggplot(aes_string(x=x,y='.',colour=colour,fill=fill,linetype=linetype,shape=shape),data=mdfr);
    p <- p + geom_bar(aes(x=x,y='.',fill=fill),data=mdfr[mdfr$modeltype=='rsim'],stat="identity",position='identity',alpha=0.5);
    p <- p + geom_line(aes(x=x,y='.',colour=colour),data=mdfr[(mdfr$modeltype=='tcsam'),],size=1);
    p <- p + ggtheme;
#    print(p);
    if (!is.null(xlab))     p <- p + xlab(xlab);
    if (!is.null(ylb))      p <- p + ylab(ylb);
    if (!is.null(title))    p <- p + ggtitle(title);
    if (!is.null(faceting)) p <- p + facet_grid(faceting);
    if (!is.null(guideTitleColour))   p <- p + guides(colour=guide_legend(title=guideTitleColour,  override.aes=list(alpha=1.0,size=6,order=1)));
    if (!is.null(guideTitleFill))     p <- p + guides(fill=guide_legend(title=guideTitleFill,      override.aes=list(alpha=1.0,size=6,order=1)));
    if (!is.null(guideTitleLineType)) p <- p + guides(linetype=guide_legend(title=guideTitleLineType,override.aes=list(alpha=1.0,size=6,order=1)));
    if (!is.null(guideTitleShape))    p <- p + guides(shape=guide_legend(title=guideTitleShape,   override.aes=list(alpha=1.0,size=6,order=1)));
    if (showPlot) print(p);
    return(p)
}