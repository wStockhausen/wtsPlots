#' 
#' @title Get the plot legend(s) 
#' @description Function to get the plot legend(s)  (based on cowplot:::get_legend)
#' @param plot - object capable of being converted to a gtable (e.g., a ggplot2 object) 
#' @param return_all - flag to return all legends
#' @return a legend grob or list of grobs.
#' @details Port of cowplot:::get_legend
#' @export
#' 
getLegend<-function(plot,return_all=FALSE){
  getPlotComponent(plot,"guide-box",return_all=return_all)
}

#' 
#' @title Get names of all plot components 
#' @description Function to get names of all plot components (based on cowplot:::plot_component_names)
#' @param plot - object capable of being converted to a gtable (e.g., a ggplot2 object) 
#' @return a vector of grob names.
#' @details Port of cowplot:::plot_component_names
#' @importFrom cowplot as_gtable
#' @export
#' 
plotComponentNames<-function (plot) {
    if (gtable::is.gtable(plot)) {
        plot$layout$name
    }
    else {
        cowplot::as_gtable(plot)$layout$name
    }
}

#' 
#' @title Get all plot components 
#' @description Function to get all plot components (based on cowplot:::get_plot_compnent)
#' @param plot - object capable of being converted to a gtable (e.g., a ggplot2 object) 
#' @return a grob or list of grobs.
#' @details Port of cowplot:::plot_components
#' @importFrom cowplot as_gtable
#' @export
#' 
plotComponents<-function (plot) {
    if (gtable::is.gtable(plot)) {
        plot$grobs
    }
    else {
        cowplot::as_gtable(plot)$grobs
    }
}
  
#' 
#' @title Get a plot component 
#' @description Function to get a plot component (based on cowplot:::get_plot_compnent)
#' @param plot - object capable of being converted to a gtable (e.g., a ggplot2 object) 
#' @param pattern - grob pattern to look for 
#' @param return_all -  flag to return all objects matching the pattern, otherwise just the first
#' @return invisibly, a grob or list of grobs matching the pattern.
#' @details Port of cowplot:::get_plot_component 
#' @export
#' 
getPlotComponent<-function (plot, pattern, return_all = FALSE) {
    plot <- as_gtable(plot)
    grob_names <- plotComponentNames(plot)
    grobs <- plotComponents(plot)
    grobIndex <- which(grepl(pattern, grob_names))
    if (length(grobIndex) != 0) {
        if (length(grobIndex) > 1 && !return_all) {
            warning("Multiple components found; returning the first one. To return all, use `return_all = TRUE`.")
            grobIndex <- grobIndex[1]
            matched_grobs <- grobs[[grobIndex]]
        }
        else if (length(grobIndex) > 1 && return_all) {
            matched_grobs <- grobs[grobIndex]
        }
        else {
            matched_grobs <- grobs[[grobIndex]]
        }
    }
    else {
        matched_grobs <- NULL
    }
    invisible(matched_grobs)
}

#' 
#' @title Turn off ggplot2 legend 
#' @description Function to turn off y-axis title
#' @return invisibly, a `ggplot2` theme obj
#' @details Shortcut to "ggplot2::theme(legend.position="none")".
#' @importFrom ggplot2 theme
#' @export
#' 
noL<-function(){return(ggplot2::theme(legend.position="none"));}

#' 
#' @title Turn off ggplot2 x-axis title 
#' @description Function to turn off x-axis title
#' @return invisibly, a `ggplot2` theme obj
#' @details Shortcut to "ggplot2::theme(axis.title.x=element_blank())".
#' @importFrom ggplot2 theme
#' @export
#' 
noXT<-function(){return(ggplot2::theme(axis.title.x=element_blank()));}

#' 
#' @title Turn off ggplot2 y-axis title 
#' @description Function to turn off y-axis title
#' @return invisibly, a `ggplot2` theme obj
#' @details Shortcut to "ggplot2::theme(axis.title.y=element_blank())".
#' @importFrom ggplot2 theme
#' @export
#' 
noYT<-function(){return(ggplot2::theme(axis.title.y=element_blank()));}

