#' 
#' @title Make diagnostic plots for a parameter in an [rstan](https://mc-stan.org/) mcmc simulation
#'  
#' @description Function to make diagnostic plots for a parameter in an [rstan](https://mc-stan.org/) mcmc simulation.
#' 
#' @param mcmc - [rstan](https://mc-stan.org/) stanfit object from running [rstan::sampling()] or [tmbstan::tmbstan()]
#' @param var - name of variable to plot 
#' @param label - label to use for variable (default=\code{var})
#' 
#' @return a ggplot2::gg object (or NULL if the [rstan](https://mc-stan.org/) package is not installed.)
#' 
#' @details Requires the [rstan](https://mc-stan.org/) and [cowplot] packages. Combines plots from running [rstan::stan_trace()], [rstan::stan_ac()] (both ACF and PACF plots),
#' [rstan::stan_hist()], and [rstan::density()] for the model parameter identified by \code{var} Plots 
#' are combined using [cowplot::plot_grid()]. The \code{label} input can be specified to provide a 
#' more informative name for the plotted parameter. 
#' 
#' @import ggplot2
#' 
#' @export
#' 
plotRStanMCMC<-function(mcmc,var,label=var){
  if (requireNamespace("rstan", quietly = TRUE)&requireNamespace("cowplot", quietly = TRUE)){
    if (class(mcmc)[1]!="stanfit") 
      stop("'mcmc' musst be a 'rstan::stanfit' object.");
    nch = dim(mcmc)[2];
    p1  = rstan::stan_trace(mcmc,var,inc_warmup=TRUE,ncol=1) + 
            ggplot2::guides(colour=ggplot2::guide_legend(ncol=ceiling(nch/4))) + 
            ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                           legend.position=c(0.99,0.99),
                           legend.justification=c(1,1),
                           legend.background=ggplot2::element_rect(fill=NA));
    
    p2  = rstan::stan_ac(mcmc,var,inc_warmup=FALSE,ncol=1) + 
            ggplot2::scale_y_continuous(limits=c(0,1),breaks=seq(from=0,to=1,by=0.5)) +
            ggplot2::scale_x_continuous(limits=c(-1,22),breaks=seq(from=0,to=20,by=5)) +
            ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                           axis.title.x=ggplot2::element_blank());
    
    p3  = rstan::stan_ac(mcmc,var,inc_warmup=FALSE,ncol=1,partial=TRUE) + 
            ggplot2::scale_y_continuous(limits=c(0,1),breaks=seq(from=0,to=1,by=0.5)) +
            ggplot2::scale_x_continuous(limits=c(-1,22),breaks=seq(from=0,to=20,by=5)) +
            ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                           axis.title.x=ggplot2::element_text(size=10));
    
    p4  = rstan::stan_hist(mcmc,var,inc_warmup=FALSE,ncol=1) + 
            ggplot2::theme(axis.title.x=ggplot2::element_blank());
    
    p5  = rstan::stan_plot(mcmc,var,inc_warmup=FALSE,ncol=1,show_density=TRUE) + 
            ggplot2::theme(axis.text.y=ggplot2::element_blank());
    
    rw2 = cowplot::plot_grid(p2,p3,ncol=1,labels=c("acf","pacf"),
                             hjust=-0.1,vjust=0.6,label_size=12);
    rw3 = cowplot::plot_grid(p4,p5,nrow=1,labels=c("histogram","density"),
                             hjust=-0.1,vjust=0.8,label_size=12);
    pg = cowplot::plot_grid(p1,rw2,rw3,ncol=1,
                            labels=c(paste0(label,": trace plot","","")),
                            hjust=-0.1,vjust=0.9,label_size=12);
    return(pg);
  } else {
    if (!requireNamespace("rstan", quietly = TRUE))
      warning("The 'rstan' package is required to use this function. Please install it.\nReturning NULL.")
    if (!requireNamespace("cowplot", quietly = TRUE))
      warning("The 'cowplot' package is required to use this function. Please install it.\nReturning NULL.")
    return(NULL);
  }
}
