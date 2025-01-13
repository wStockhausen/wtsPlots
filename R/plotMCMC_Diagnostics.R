var = "log_PE";
#' 
#' @title Make diagnostic plots for a parameter in an [rstan](https://mc-stan.org/) mcmc simulation
#'  
#' @description Function to make diagnostic plots for a parameter in an [rstan](https://mc-stan.org/) mcmc simulation.
#' 
#' @param mcmc - mcmc object compatible with [basyesplot] (i.e., from running [rstan::sampling()] or [tmbstan::tmbstan()])
#' @param var - name of variable to plot 
#' @param label - label to use for variable (default=\code{var})
#' 
#' @return a ggplot2::gg object (or NULL if the [bayesplot](https://mc-stan.org/bayesplot/index.html) package is not installed.)
#' 
#' @details Requires the [bayesplot](https://mc-stan.org/bayesplot/index.html) and [cowplot] packages. 
#' Combines plots from running [bayesplot::mcmc_trace()], [bayesplot::mcmc_acf_bar()],
#' [bayesplot::mcmc_hist()], and [bayesplot::mcmc_areas()] for the model parameter identified 
#' by \code{var} Plots are combined using [cowplot::plot_grid()]. The \code{label} input 
#' can be specified to provide a more informative name for the plotted parameter. 
#' 
#' @import ggplot2
#' 
#' @md 
#' 
#' @export
#' 
plotMCMC_Diagnostics<-function(mcmc,var,label=var){
  if (requireNamespace("bayesplot", quietly = TRUE) & 
      requireNamespace("cowplot",   quietly = TRUE) & 
      requireNamespace("posterior", quietly = TRUE)){
    rHat = formatC(bayesplot::rhat(posterior::extract_variable_matrix(mcmc,var)),
                   digits=3,format="f");
    ESS  = formatC(posterior::ess_tail(posterior::extract_variable_matrix(mcmc,var)),
                   digits=0,format="f",big.mark=",");
    p1 = bayesplot::mcmc_trace(mcmc,pars=var,np=bayesplot::nuts_params(mcmc)) + 
            ggplot2::ggtitle(label) + 
            ggplot2::guides(colour=ggplot2::guide_legend(ncol=ceiling(nch/4))) + 
            ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                           plot.title=ggplot2::element_text(hjust=0.5),
                           legend.position=c(0.99,-0.1),
                           legend.justification=c(1,1),
                           legend.text=element_text(size=10),
                           legend.background=ggplot2::element_rect(fill=NA));
    
    p2 = bayesplot::mcmc_acf_bar(posterior::merge_chains(posterior::as_draws(mcmc)),var) + 
            ggplot2::labs(subtitle=paste0("Rhat =",rHat,"   ESS(tail) =",ESS)) +
            ggplot2::scale_y_continuous(limits=c(0,1),breaks=seq(from=0,to=1,by=0.5)) +
            ggplot2::scale_x_continuous(limits=c(-1,22),breaks=seq(from=0,to=20,by=5)) +
            ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                           axis.title.x=ggplot2::element_text(size=10),
                           strip.text=ggplot2::element_blank(),
                           plot.title=ggplot2::element_blank(),
                           plot.subtitle=ggplot2::element_text(size=10,hjust=0.5),
                           plot.caption=ggplot2::element_blank());
    
    p3 = bayesplot::mcmc_hist(mcmc,pars=var) + 
            ggplot2::theme(axis.title.x=ggplot2::element_blank());
    
    p4 = bayesplot::mcmc_areas(mcmc,pars=var,prob=0.8) + 
            ggplot2::theme(axis.text.y=ggplot2::element_blank());
    
    rw3 = cowplot::plot_grid(p3,p4,nrow=1,labels=c("histogram","density"),
                             hjust=-0.1,vjust=0.8,
                             label_size=12,label_fontface="plain");
    pg = cowplot::plot_grid(p1,p2,rw3,ncol=1,
                            labels=c("trace plot","acf",""),
                            hjust=c(-0.1,-0.1,0),vjust=0.9,
                            label_size=12,label_fontface="plain");
    return(pg);
  } else {
    if (!requireNamespace("cowplot", quietly = TRUE))
      warning("The 'cowplot' package is required to use this function. Please install it.\nReturning NULL.")
    if (!requireNamespace("bayesplot", quietly = TRUE))
      warning("The 'bayesplot' package is required to use this function. Please install it.\nReturning NULL.")
    if (!requireNamespace("posterior", quietly = TRUE))
      warning("The 'posterior' package is required to use this function. Please install it.\nReturning NULL.")
    return(NULL);
  }
}
# p = plotMCMCdiagnostics(mcmc,var)
# print(p);
# p = plotMCMCdiagnostics(mcmc,"log_biomass_pred[49]","ln-scale terminal biomass")
# print(p);
