#'
#' @title Plot melted dataframe using a bubble plot format
#'
#' @description Function to plot a melted dataframe using a bubble plot format.
#'
#'@param x - column name for x-axis values
#'@param y - column name for y-axis values
#'@param value.var - column name for values to aggregate (value.var in cast)/plot as circles
#'@param agg.formula - aggregation formula (left-hand side of cast formula)
#'@param agg.function - aggregation function (fun.aggregate in cast)
#'@param ... - further arguments passed to aggregating function
#'@param colour - column name to which colour aesthetic is mapped
#'@param facet_grid - faceting formula
#'@param xlab - x axis label
#'@param ylab - y axis label
#'@param title - plot title
#'@param guideTitleSize - title for bubble size scale
#'@param alpha - transparency level
#'@param maxBubbleSize - max bubble size
#'@param useColourGradient - flag (T/F) to use a color gradient for bubble color
#'@param colourPalette - colour palette for colour gradient (default=\code{wtsUtilities::createColorPalette('jet',100,alpha=alpha)})
#'@param guideTitleSize - title for size guide
#'@param guideTitleColour - title for colour guide
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
plotMDFR.Bubbles<-function(mdfr,
                           x=NULL,
                           y=NULL,
                           value.var='val',
                           agg.formula=NULL,
                           agg.function=sum,
                           ...,
                           colour=NULL,
                           facet_grid=NULL,
                           xlab="",
                           ylab="",
                           title="",
                           guideTitleSize="",
                           alpha=0.5,
                           maxBubbleSize=6,
                           useColourGradient=FALSE,
                           colourPalette=wtsUtilities::createColorPalette('jet',100,alpha=alpha),
                           guideTitleColour="",
                           ggtheme=ggplot2::theme_gray(),
                           showPlot=FALSE
                           ){
    #cast melted dataframe
    if (!is.null(agg.formula)){
        #aggregate using formula
        form<-paste(agg.formula,".",sep="~")
        mdfr<-reshape2::dcast(mdfr,form,fun.aggregate=agg.function,value.var=value.var);
        names(mdfr)[ncol(mdfr)]<-value.var;
    }
  
    #plot resulting dataframe
    if (sum(mdfr[[value.var]])==0) mdfr[[value.var]][1]<-1.0e-10;
    p <- ggplot(aes_string(x=x,y=y,size=value.var,colour=colour),data=mdfr);
    p <- p + scale_size_area(max_size=maxBubbleSize);
    p <- p + ggtheme;
    if (useColourGradient) p <- p + scale_color_gradientn(colours=colourPalette)
    p <- p + geom_point(alpha=alpha);
    if (!is.null(xlab))     p <- p + xlab(xlab);
    if (!is.null(ylab))     p <- p + ylab(ylab);
    if (!is.null(title))    p <- p + ggtitle(title);
    if (!is.null(facet_grid)) p <- p + facet_grid(facet_grid);
    p <- p + guides(size=guide_legend(title=guideTitleSize,override.aes=list(alpha=1.0),order=1));
    if (!is.null(guideTitleColour)) {
        if (useColourGradient) {
            p <- p + guides(colour=guide_colorbar(guideTitleColour,alpha=1.0,order=2));
        } else {
            p <- p + guides(colour=guide_legend(guideTitleColour,override.aes=list(alpha=1.0,size=6),order=2));
        }
    }
    if (showPlot) print(p);

    return(p)
}

