#' 
#' @title Get standard plot theme
#' 
#' @description Function to get the standard plot theme
#' 
#' @return \pkg{ggplot2} theme object with "standard" thematics
#' 
#' @import ggplot2
#' 
#' @export
#' 
getStdTheme<-function(){
    std_theme = ggplot2::theme(plot.background =ggplot2::element_blank(),
                               panel.background=ggplot2::element_blank(),
                               panel.border    =ggplot2::element_rect(colour="black",fill=NA),
                               panel.grid      =ggplot2::element_blank(),
                               panel.spacing   =unit(0,units="cm"));
    return(std_theme);
}