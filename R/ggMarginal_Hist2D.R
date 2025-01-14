#' @title Add marginal histograms to ggplot2 2d histogram plots
#'
#' @description Create a ggplot2 2d histogram plot with marginal histogram plots.
#'
#' @note The \code{grid}, \code{gtable}, and \code{rlang} packages are required for this
#' function.
#' @param data The data.frame to use for creating the plots.
#' @param x The name of the variable along the x axis.
#' @param y The name of the variable along the y axis.
#' @param size Integer describing the relative size of the marginal plots
#' compared to the main plot. A size of 5 means that the main plot is 5x wider
#' and 5x taller than the marginal plots.
#' @param stat_var - name of statistical variable to plot; options=("count","ncount","density","ndensity")
#' @param hist_scale - scale for histograms; options = ("arithmetic","log10","4th-root")
#' @param bins - number of bins to use (scalar or 2-element vector); default=30
#' @param binwidths - bin widths to use (scalar or 2-element vector); default=NULL
#' @param xlab - x-axis title; default=NULL (i.e., use ggplot2 default)
#' @param ylab - y-axis title; default=NULL (i.e., use ggplot2 default)
#' @param xparams - list of [ggplot2::scale_x_continuous()] parameters; default = 
#' list(limits=NULL,breaks=waiver(),labels=waiver(),oob=scales::squish)
#' @param yparams - list of [ggplot2::scale_y_continuous()] parameters; default = 
#' list(limits=NULL,breaks=waiver(),labels=waiver(),oob=scales::squish)
#' @param fill_scale - 2d histogram fill scale; default=ggplot2::scale_fill_viridis_c(option="magma",name="")
#' @param legend.position - legend position; default=c(0.99,0.99)
#' @param legend.justification - legend justification; default=c(1,1)
#' @param ... Extra parameters to pass to the marginal plots. Any parameter that
#' [ggplot2::geom_histogram()] accepts can be used. 
#' For example, \code{colour = "red"} can be used for any marginal plot type,
#' @param groupColour If \code{TRUE}, the colour (or outline) of the marginal
#' plots will be grouped according to the variable mapped to \code{colour} in the
#' scatter plot. The variable mapped to \code{colour} in the scatter plot must
#' be a character or factor variable. See examples below.
#' @param groupFill If \code{TRUE}, the fill of the marginal
#' plots will be grouped according to the variable mapped to \code{colour} in the
#' scatter plot. The variable mapped to \code{colour} in the scatter plot must
#' be a character or factor variable.
#' @param addValues - flag to add histograam values to 2d matrix? (default = TRUE)
#' @param addGrid - flag to add grids to histograms (default=TRUE)
#' @param testing - flag (T/F) to include 
#' 
#' @return If \code{testing} is false, a \code{ggGTbl} object that can be printed using S3 dispatch with \code{print}.
#' If \code{testing} is TRUE, a list with elements \cr
#' \itemize{
#'  \item{ggGTbl - an object of class \code{ggGTbl}.}
#'  \item{built - a list with elements p2d, px,py corresponding to ggplot2 objects "built" with [ggplot2::ggplot_build()]}
#' }
#' 
#' @note This function and related code are based on the [ggExtra::ggMarginal()] 
#' code developed by Dean Attali ("daattali@gmail.com").
#' 
#' @examples
#' library(ggplot2)
#' ggGTbl = ggMarginal_Hist2D(data=mtcars,x=wt,y=mpg,
#'                           hist_scale="arithmetic",
#'                           binwidths=c(0.5,7),
#'                           xparams=list(limits=c(0, 3),breaks=seq(0, 7,0.5),oob=scales::squish),
#'                           yparams=list(limits=c(0,40),breaks=seq(0,40,2),oob=scales::squish),
#'                           legend.position=c(0.01,0.99),
#'                           legend.justification=c(0,1),
#'                           showIntermediates=FALSE);
#' print(ggGTbl,newpage=TRUE);
#' gglst = ggMarginal_Hist2D(data=mtcars,x=wt,y=mpg,
#'                           hist_scale="arithmetic",
#'                           binwidths=c(0.5,7),
#'                           xparams=list(limits=c(0, 3),breaks=seq(0, 7,0.5),oob=scales::squish),
#'                           yparams=list(limits=c(0,40),breaks=seq(0,40,2),oob=scales::squish),
#'                           legend.position=c(0.01,0.99),
#'                           legend.justification=c(0,1),
#'                           showIntermediates=FALSE,
#'                           testing=TRUE);
#' print(gglst$ggGTbl,newpage=TRUE);
#'
#' @import ggplot2 
#' @importFrom grid unit
#' @importFrom scales squish
#' @export
ggMarginal_Hist2D <- function(
                       data, x, y, size = 5,
                       after_stat_var=c("ncount","count","density","ndensity"),
                       hist_scale = c("arithmetic","log10","4th-root"),
                       bins = 30,
                       binwidths=NULL,
                       xlab=NULL, ylab=NULL,
                       xparams=list(limits=NULL, breaks=waiver(), labels=waiver(), name=waiver(), oob=scales::squish),
                       yparams=list(limits=NULL, breaks=waiver(), labels=waiver(), name=waiver(), oob=scales::squish),
                       fill_scale=ggplot2::scale_fill_viridis_c(option="magma",name=""),
                       legend.position=c(0.99,0.99),
                       legend.justification=c(1,1),
                       ..., 
                       groupColour = FALSE, 
                       groupFill = FALSE,
                       showIntermediates=FALSE,
                       addValues=TRUE,
                       addGrid=TRUE,
                       testing=FALSE) {
  if (length(bins)==1)      bins=rep(bins,2);
  if (length(binwidths)==1) binwidths=rep(binwidths,2);
  #--apply limits as necessary
  if (!is.null(xparams$limits)) 
    data = data |> dplyr::filter(dplyr::between({{x}},xparams$limits[1],xparams$limits[2]))
  if (!is.null(yparams$limits)) 
    data = data |> dplyr::filter(dplyr::between({{y}},yparams$limits[1],yparams$limits[2]))
  #--create 2d histogram 
  after_stat_var = rlang::arg_match(after_stat_var);
  hist_scale     = rlang::arg_match(hist_scale);
  if (hist_scale=="arithmetic") {
    p2d = ggplot(data,aes({{x}},{{y}},fill=ggplot2::after_stat(.data[[after_stat_var]])));
  } else if (hist_scale=="log10") {
    p2d = ggplot(data,aes({{x}},{{y}},fill=log10(ggplot2::after_stat(.data[[after_stat_var]]))));
  } else if (hist_scale=="4th-root") {
    p2d = ggplot(data,aes({{x}},{{y}},fill=(ggplot2::after_stat(.data[[after_stat_var]])^0.25)));
  }
  p2d = p2d +
        stat_bin_2d(bins=bins,binwidth=binwidths,drop=TRUE) + 
        fill_scale; 
  if (addGrid) {
    p2d = p2d + 
            stat_bin_2d(bins=bins,binwidth=binwidths,drop=TRUE,fill=NA,colour="white");
  }
  if (addValues){
    p2d = p2d + 
            stat_bin_2d(aes(label=ggplot2::after_stat(.data[[after_stat_var]])),
                        geom="text",bins=bins,binwidth=binwidths,drop=TRUE,fill=NA,colour="white");
  }
  p2d = p2d + do.call(scale_x_continuous,xparams);
  p2d = p2d + do.call(scale_y_continuous,yparams);
  p2d = p2d + 
        theme(legend.position=legend.position,
              legend.justification=legend.justification) + 
        wtsPlots::getStdTheme() +
        ggplot2::theme(plot.margin = grid::unit(c(0, 0, .25, .25), "cm"))#--(t,r,b,l)
  if (!is.null(xlab)) p2d = p2d + xlab(xlab);
  if (!is.null(ylab)) p2d = p2d + ylab(ylab);
  if (showIntermediates) print(p2d);
  #--build plot to "realize" objects
  blt_p2d = ggplot2::ggplot_build(p2d);

  #--Pull out the plot title/subtitle if one exists,
  #----save it as a grob for later use
  labels <- blt_p2d$plot$labels
  hasTitle <- (!is.null(labels$title) || !is.null(labels$subtitle))
  if (hasTitle) {
    titleGrobs <- getTitleGrobs(p2d)
    p2d$labels$title <- NULL
    p2d$labels$subtitle <- NULL
  }

  #--Create the margin plots
  #----upper plot (x-axis)
  if (hist_scale=="arithmetic") {
    px = ggplot(data, aes(x={{x}},y=ggplot2::after_stat(.data[[after_stat_var]])));
  } else if (hist_scale=="log10") {
    px = ggplot(data, aes(x={{x}},y=log10(ggplot2::after_stat(.data[[after_stat_var]]))));
  } else if (hist_scale=="4th-root") {
    px = ggplot(data, aes(x={{x}},y=ggplot2::after_stat(.data[[after_stat_var]])^0.25));
  }
  px = px + geom_histogram(bins=bins[1],binwidth=binwidths[1],boundary=0,
                           alpha=0.5,fill="grey50");
  if (addGrid) 
    px = px + geom_histogram(bins=bins[1],binwidth=binwidths[1],boundary=0,
                           alpha=0.5,fill=NA,colour="white");
  if (addValues){
    px = px + 
           stat_bin(aes(label=ggplot2::after_stat(.data[[after_stat_var]])),
                    geom="text",bins=bins[1],binwidth=binwidths[1],boundary=0,
                    colour="black",vjust=1);
  }
  px = px + do.call(scale_x_continuous,xparams);
  px = px + wtsPlots::getStdTheme();
  if (showIntermediates) print(px);
  blt_px = ggplot2::ggplot_build(px);
  #----righthand plot (y-axis)
  if (hist_scale=="arithmetic") {
    py = ggplot(data, aes(y={{y}},x=ggplot2::after_stat(.data[[after_stat_var]])));
  } else if (hist_scale=="log10") {
    py = ggplot(data, aes(y={{y}},x=log10(ggplot2::after_stat(.data[[after_stat_var]]))));
  } else if (hist_scale=="4th-root") {
    py = ggplot(data, aes(y={{y}},x=ggplot2::after_stat(.data[[after_stat_var]])^0.25));
  }
  py = py + geom_histogram(bins=bins[2],binwidth=binwidths[2],boundary=0,
                           alpha=0.5,fill="grey50");
  if (addGrid) 
    py = py + geom_histogram(bins=bins[2],binwidth=binwidths[2],boundary=0,
                           alpha=0.5,fill=NA,colour="white");
  if (addValues){
    py = py + 
           stat_bin(aes(label=ggplot2::after_stat(.data[[after_stat_var]])),
                    geom="text",bins=bins[2],binwidth=binwidths[2],boundary=0,
                    colour="black",vjust=1,angle=270);
  }
  py = py + do.call(scale_y_continuous,yparams);
  py = py + wtsPlots::getStdTheme();
  if (showIntermediates) print(py);
  blt_py = ggplot2::ggplot_build(py);

  # #--Now add the marginal plots to the 2d histogram plot
  p2dGrob = ggplot2::ggplotGrob(p2d);#--create plot grob
  ggTmp1  = addTopMargPlot(p2dGrob,  px, size);
  ggTmp2  = addRightMargPlot(ggTmp1, py, size);
  
  # Add a class for S3 method dispatch for printing the plot gtable
  class(ggTmp2) <- c("ggGTbl", class(ggTmp2));
  if (testing)
    return(list(ggGTbl=ggTmp2,built=list(p2d=blt_p2d,px=blt_px,py=blt_py)));
  return(ggTmp2);
}

#' @title Print a \code{ggGTbl} object
#'
#' @description \code{ggGTbl} objects are created from \code{ggMarginal_XX} functions. 
#' This is the S3 generic print method to print the ggGTbl result of the 
#' function.
#'
#' @param x - ggGTbl object.
#' @param newpage - flag to draw a new (empty) page before the ggTbl object is drawn?
#' @param ... ignored
#' 
#' @note This function is based on the [ggExtra::ggMarginal()] 
#' code developed by Dean Attali ("daattali@gmail.com").
#' @seealso [ggMarginal_Hist2D]
#' @export
#' @keywords internal
print.ggGTbl <- function(x, newpage = grDevices::dev.interactive(), ...) {
  if (newpage) grid::grid.newpage()
  if (isTRUE(getOption("rstudio.notebook.executing"))) {
    x <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::annotation_custom(x) +
      ggplot2::theme_void()
    print(x)
  } else {
    grid::grid.draw(x)
  }
}

#' @title Pull out the title and subtitle grobs for a ggplot2 plot
#' 
#' @description Function to pull out the title and subtitle grobs for a ggplot2 plot, after check to
#' make sure there is a title. 
#' @param p - plot
#' @details Copied from \code{ggExtra}. Created by Dean Attali ("daattali@gmail.com").
#' @importFrom ggplot2 ggplotGrob
#' @keywords internal
#' 
getTitleGrobs <- function(p) {
  grobs <- ggplot2::ggplotGrob(p)$grobs
  gindTitle <- vapply(
    grobs, function(x) grepl(pattern = "plot\\.title", x$name), logical(1)
  )
  gindSub <- vapply(
    grobs, function(x) grepl(pattern = "plot\\.subtitle", x$name), logical(1)
  )
  list(
    titleG = grobs[gindTitle][[1]],
    subTitleG = grobs[gindSub][[1]]
  )
}

#' @title add a marginal plot to the top of another plot 
#' @description Function to add a marginal plot to the top of another plot. 
#' @param ggMargGrob - plot grob to add marginal plot to
#' @param top - marginal plot to add
#' @param size - relative height of main plot to marginal plot
#' 
#' @details Copied from \code{ggExtra}. Created by Dean Attali ("daattali@gmail.com"). 
#' 
#' @importFrom grid unit
#' @importFrom gtable gtable_add_rows
#' @importFrom gtable gtable_add_grob
#' @keywords internal
#' 
addTopMargPlot <- function(ggMargGrob, top, size) {
  panelPos <- getPanelPos(ggMargGrob)
  topMargG <- getMargGrob(top)
  gt <- gtable::gtable_add_rows(
    x = ggMargGrob,
    heights = grid::unit(1 / size, "null"), pos = 0
  )
  gtable::gtable_add_grob(
    x = gt, grobs = topMargG, t = 1, b = 1,
    l = panelPos[["l"]], r = panelPos[["r"]],
    z = Inf, clip = "on", name = "topMargPlot"
  )
}

#' @title add a marginal plot to the right side of another plot 
#' 
#' @description Function to add a marginal plot to the right of another plot. 
#' 
#' @param ggMargGrob - plot grob to add marginal plot to
#' @param right - marginal plot to add
#' @param size - relative width of main plot to marginal plot
#' 
#' @details Copied from \code{ggExtra}. Created by Dean Attali ("daattali@gmail.com").
#' 
#' @importFrom grid unit
#' @importFrom gtable gtable_add_cols
#' @importFrom gtable gtable_add_grob
#' @keywords internal
#' 
addRightMargPlot <- function(ggMargGrob, right, size) {
  panelPos   = getPanelPos(ggMargGrob)
  rightMargG = getMargGrob(right)
  gt <- gtable::gtable_add_cols(
    x = ggMargGrob,
    widths = grid::unit(1 / size, "null"),
    pos = -1
  )
  gtable::gtable_add_grob(
    x = gt, grobs = rightMargG, t = panelPos[["t"]],
    b = panelPos[["b"]], r = ncol(gt), l = ncol(gt),
    z = Inf, clip = "on", name = "rightMargPlot"
  )
}

#' @title Get the panel position
#' 
#' @description Function to add a grob to the top of a gtable. 
#' 
#' @param gtbl - a gtable
#' 
#' @return Position of panel (t,l,b,r).
#' 
#' @details Copied from \code{ggExtra}. Created by Dean Attali ("daattali@gmail.com").
#' 
#' @keywords internal
#' 
getPanelPos <- function(gtbl) {
  layDF <- gtbl$layout
  layDF[layDF$name == "panel", c("t", "l", "b", "r")]
}

#' @title Get the panel grob from a ggplot2 plot object
#' 
#' @description Function to get the panel grob from a ggplot2 plot object. 
#' 
#' @param plt - ggplot2 plot object
#' 
#' @return a gtable with the panel grob only.
#' 
#' @details Modified from \code{ggExtra}. Created by Dean Attali ("daattali@gmail.com").
#' 
#' @importFrom ggplot2 ggplotGrob
#' @importFrom gtable gtable_filter
#' @keywords internal
#' 
getMargGrob <- function(plt) {
  grb <- ggplot2::ggplotGrob(plt)
  gtable::gtable_filter(grb, pattern = "panel")
}

#' @title Add the title/subtitle grobs to the main plot grob, along with a little padding
#' 
#' @description Function to add the title/subtitle grobs to the main plot grob, 
#' along with a little padding. 
#' 
#' @param gtbl - gtable to add title grobs to
#' @param titleGrobs - grobs to add to gtable
#' @param l - left size
#' @param r - right size
#' 
#' @details Modified from \code{ggExtra}. Created by Dean Attali ("daattali@gmail.com").
#' 
#' @importFrom grid gpar
#' @importFrom grid rectGrob
#' @importFrom grid unit
#' @importFrom gtable gtable_add_rows
#' @importFrom gtable gtable_add_grob
#' @keywords internal
#' 
addTitleGrobs <- function(gtbl, titleGrobs) {
  layout <- gtbl$layout
  l <- layout[layout$name == "panel", "l"]
  spacerGrob <- grid::rectGrob(
    height = grid::unit(.2, "cm"),
    gp = grid::gpar(col = "white", fill = NULL)
  )
  plotWSpace <- rbindGrobs(
    topGrob = spacerGrob, gtable = gtbl,
    l = l, r = l
  )
  plotWSubTitle <- rbindGrobs(
    topGrob = titleGrobs$subTitleG,
    gtable = plotWSpace, l = l, r = l
  )
  rbindGrobs(
    topGrob = titleGrobs$titleG,
    gtable = plotWSubTitle, l = l, r = l
  )
}

#' @title Add a grob to the top of a gtable 
#' 
#' @description Function to add a grob to the top of a gtable. 
#' 
#' @param topGrob - grob to add to gtable as top row
#' @param gtable - gtable to be added to
#' @param l - left size
#' @param r - right size
#' 
#' @details Copied from \code{ggExtra}. Created by Dean Attali ("daattali@gmail.com").
#' 
#' @importFrom grid grobHeight
#' @importFrom gtable gtable_add_rows
#' @importFrom gtable gtable_add_grob
#' @keywords internal
#' 
rbindGrobs <- function(topGrob, gtable, l, r) {
  topH <- grid::grobHeight(topGrob)
  gt_t <- gtable::gtable_add_rows(x = gtable, heights = topH, pos = 0)
  gtable::gtable_add_grob(
    x = gt_t, grobs = topGrob, t = 1, b = 1,
    l = l, r = r, z = Inf
  )
}

