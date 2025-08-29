#' 
#' @title Create a [DHARMa] QQ plot as a [ggplot2] object
#' @description Function to create a [DHARMa] QQ plot as a [ggplot2] object.
#' @param obj - an object of class 'DHARMa' 
#' @param colors - vector of same length as the scaled residuals to use for color-scaling (or NULL)
#' @param showLegend - flag to show color/fill legend
#' @param shape - ggplot2 shape code (default = 23)
#' @return list with [ggplot2] plot object and uniformity, dispersion, and outliers tests results
#' @details Inspired by the R graphics returned by [DHARMa::plotQQunif], but returned as a ggplot2 plot.
#' @export 
#' 
plotDHARMaQQ<-function(obj,colors=NULL,showLegend=FALSE,shape=23){
  if (inherits(obj,"DHARMa")){
    tbl = tibble::tibble(y=obj$scaledResiduals);
    if (!is.null(colors)) tbl$colors = colors;
    tbl = tbl |> dplyr::arrange(y);
    n <- nrow(tbl);
    tbl$x = seq_len(n)/(n + 1);
    unif = DHARMa::testUniformity(obj, plot = FALSE);
    outl = DHARMa::testOutliers(obj, plot = FALSE);
    disp = DHARMa::testDispersion(obj,plot=FALSE);
    sub = paste0("uniformity test p.value = ",prettyNum(round(unif$p.value,digits=4)),"\n",
                 "outliers test p.value   = ",prettyNum(round(outl$p.value,digits=4)),"\n",
                 "dispersion test p.value = ",prettyNum(round(disp$p.value,digits=4)),"\n");
  } else {
    stop("obj in plotDHARMaQQ needs to be of class 'DHARMa'.")
  }
  if (is.null(colors)){
    p = ggplot(tbl,aes(x=x,y=y));
  } else {
    p = ggplot(tbl,aes(x=x,y=y,color=colors));
  }
  p = p + 
        annotate("text",x=0.01,y=0.99,label=sub,hjust=0,vjust=1,lineheight=0.8) + 
        geom_abline(slope=1,color="red",linetype=3) + 
        geom_point(shape=shape,alpha=0.2,show.legend=showLegend);
  if (is.numeric(colors)){
    p = p + scale_color_distiller(type="div");
  } else {
    p = p + scale_color_discrete();
  }
  p = p + scale_x_continuous(limits=c(0,1),name="Predicted (rank transformed)",breaks=c(0,0.2,0.4,0.6,0.8,1.0)) + 
          scale_y_continuous(limits=c(0,1),name="Observed (scaled)",breaks=c(0,0.2,0.4,0.6,0.8,1.0));
  if (showLegend) p = p + guides(colour=guide_legend(override.aes=list(alpha=1)));
  p = p + wtsPlots::getStdTheme();
  return(list(p=p,uniformity=unif,outliers=outl,dispersion=disp));
}

#' 
#' @title Create a [DHARMa] residuals plot as a [ggplot2] object
#' @description Function to create a [DHARMa] residuals plot as a [ggplot2] object.
#' @param obj - an object of class 'DHARMa' 
#' @param colors - vector of colors
#' @param showLegend - flag to show color/fill legend
#' @param quantreg - 
#' @param rank - 
#' @param asFactor - 
#' @param smoothScatter - 
#' @param quantiles - vector of quantiles to plot as regression lines
#' @return [ggplot2] plot object
#' @details Inspired by the R graphics returned by [DHARMa::plotResiduals], but returned as a ggplot2 plot.
#' @export 
#' 
plotDHARMaResiduals<-function(obj, colors=NULL, showLegend=FALSE,
                              quantreg = NULL, rank = TRUE, 
                              asFactor = NULL, smoothScatter = NULL, 
                              quantiles = c(0.25, 0.5, 0.75)){
  if (inherits(obj,"DHARMa")){
    resd = obj$scaledResiduals;        #residuals scaled 0->1
    pred = DHARMa:::ensurePredictor(obj); #--predictions from fitted model
    if (!is.factor(pred)) {            #--rank predictions
        if (rank == TRUE) {
            pred = rank(pred, ties.method = "average");
            pred = pred/max(pred);
        }
        nuniq = length(unique(pred));
        ndata = length(pred);
        if (is.null(asFactor)) 
            asFactor = (nuniq == 1) | (nuniq < 10 & ndata/nuniq > 
                10)
        if (asFactor) 
            pred = factor(pred);
    }
    qtls = DHARMa::testQuantiles(resd,pred,quantiles=quantiles,plot=FALSE);
    ttl = ifelse(qtls$p.value<=0.05,
                 "significant quantile deviations detected.",
                 "no significant quantile deviations detected.");
  } else {
    stop("obj in plotDHARMaResiduals needs to be of class 'DHARMa'.")
  }
  dfr = data.frame(x=pred,
                   y=resd);
  if (!is.null(colors)) dfr = dfr |> dplyr::bind_cols(colors=colors);
  ss = smooth.spline(pred, resd, df = 10);
  dfr1 = data.frame(x=ss$x,y=ss$y,colors="black");#--smooth spline
  lst = list();
  for (i in 1:length(quantiles)){
    lst[[i]] = tibble::tibble(
                   id=i,
                   x=qtls$predictions$pred,
                   y=qtls$predictions[[2*i]],
                   ymin=qtls$predictions[[2*i]]-qtls$predictions[[2*i+1]],
                   ymax=qtls$predictions[[2*i]]+qtls$predictions[[2*i+1]]
               );
  }
  dfrp = dplyr::bind_rows(lst);                      #--quantiles
  if (is.null(colors)){
    p = ggplot(dfr,aes(x=x,y=y));
  } else {
    dfr$colors = colors;
    p = ggplot(dfr,aes(x=x,y=y,color=colors));
  }
  p = p + 
        geom_hline(yintercept=0.25,color="red",linetype=3,show.legend=FALSE) + 
        geom_hline(yintercept=0.50,color="red",linetype=3,show.legend=FALSE) + 
        geom_hline(yintercept=0.75,color="red",linetype=3,show.legend=FALSE) + 
        geom_point(shape=22,fill=NA,alpha=0.5,show.legend=showLegend) + 
        geom_line(data=dfr1,linetype=2,show.legend=FALSE);
  for (i in 1:length(quantiles)) {
    clr = "grey";
    if (qtls$pvals[i] <= 0.05) clr = "red";
    dfrpp = dfrp |> dplyr::filter(id==i);
    p = p + geom_ribbon(aes(x=x,ymin=ymin,ymax=ymax),data=dfrpp,colour=NA,fill=clr,alpha=0.3)+
            geom_line(aes(x=x,y=y),data=dfrpp,color=clr,show.legend=FALSE);
  }
  p = p + 
        scale_x_continuous(limits=c(0,1),expand=c(0,0),breaks=c(0,0.2,0.4,0.6,0.8,1.0),name="Model Predictions (rank transformed)") + 
        scale_y_continuous(limits=c(0,1),expand=c(0,0),breaks=c(0,0.2,0.4,0.6,0.8,1.0),name="DHARMa residuals") + 
        labs(subtitle=ttl) + 
        wtsPlots::getStdTheme();
  return(p);
}

#' 
#' @title Create a combined [DHARMa] residuals and QQ plot as a [ggplot2] object
#' @description Function to create a combined [DHARMa] residuals and QQ plot as a [ggplot2] object.
#' @param obj - an object of class "DHARMa"
#' @param colors - vector of colors
#' @param showLegend - flag to show color/fill legend
#' @param quantreg - 
#' @param rank - 
#' @param asFactor - 
#' @param smoothScatter - 
#' @param quantiles - vector of quantiles to plot as regression lines
#' @param shape - [ggplot2] shape code for points
#' @param landscape - flag to orient plots in landscape format (default=FALSE)
#' @return list with [ggplot2] plot object and uniformity, dispersion, and outliers tests results
#' @details Inspired by the R graphics returned by [DHARMa::plotDHARMa], but returned as a ggplot2 plot.
#' @importFrom cowplot plot_grid
#' @export 
#' 
plotDHARMa<-function(obj, colors=NULL, showLegend=FALSE,
                      quantreg = NULL, rank = TRUE, 
                      asFactor = NULL, smoothScatter = NULL, 
                      quantiles = c(0.25, 0.5, 0.75),
                     shape=23,
                     landscape=FALSE){
  noL = theme(legend.position="none");
  lst  = plotDHARMaQQ(obj,colors,showLegend,shape);
  p2   = plotDHARMaResiduals(obj, colors,showLegend,quantreg,rank, 
                            asFactor,smoothScatter,quantiles);
  if (landscape){
    lg = wtsPlots::getLegend(lst$p);
    pg = cowplot::plot_grid(lst$p+noL,p2+noL,nrow=1);
    if (showLegend)
      pg = cowplot::plot_grid(pg,lg,nrow=1,rel_widths=c(4,1));
  } else {
    lg = wtsPlots::getLegend(lst$p);
    p1 = lst$p + noL + theme(axis.title.x=element_blank());
    p2 = p2 + noL
    pg = cowplot::plot_grid(p1,p2,ncol=1);
    pg = cowplot::plot_grid(pg,lg,ncol=2,rel_widths=c(4,1));
  }
  return(list(p=pg,uniformity=lst$uniformity,outliers=lst$outliers,dispersion=lst$dispersion));
}





