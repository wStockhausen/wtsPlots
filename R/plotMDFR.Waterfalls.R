plotMDFR.Waterfalls<-function(
                        mdfr,
                        x,y,z,
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
                       verbose=FALSE){
  x<-"z";
  y<-"y";
  z<-"val";
  library(ggplot2)
  
  zf<-1;#scaling factor
  
  yscl<-1;
  
  dfr<-mdfr[mdfr$y>2010,];
  zscl <- 2.0/max(dfr[,z],na.rm=TRUE);
  dfr$ymin<-dfr[,y];
  dfr$ymax<-dfr$ymin + zscl*dfr[,z];
  dfr$grp <-as.factor(dfr[,y]);
  
  dfro<-dfr[dfr$type=="observed",]
  dfrp<-dfr[dfr$type=="predicted",]
  
  p <- ggplot(mapping=aes_string(x=x,ymin="ymin",ymax="ymax",group="grp"));
  p <- p + geom_ribbon(data=dfro,alpha=0.5,fill="red");
  p <- p + geom_ribbon(data=dfrp,alpha=0.5,fill="blue");
  p <- p + facet_grid(rows="m~x")
  print(p);
  return(p);
}