#'
#' @title Create or add to a circle plot of age or size comp.s.
#'
#' @description Creates or adds to a circle plot of age or size comp.s.
#'
#'@param z - vector or matrix of values to be plotted as circles
#'@param x - x coordinates of circle centers
#'@param y - y coordinates of circle centers
#'@param transform - flag (T/F) to scale z values to circle area (T) or radius (F) 
#'@param overplot - flag (T/F) to plot over an existing plot
#'@param maxRadius - max radius (in data units) to allow
#'@param scale - scale factor to normalize z values by 
#'@param fg - vector of colors for circle outlines ("black","grey"),
#'@param bg - vector of colors for circle fills ("blue","green"),
#'@param lty - vector of line types for circle outlines ("solid","dotted") to use
#'@param transparency - alpha level for transparency (0-1)
#'@param plotCohorts flag (T/F) to plot cohorts
#'@param units - label for units
#'@param main - title for plot
#'@param subtitle - subtitle for plot
#'@param xlims - x axis limts
#'@param ylims - y axis limits
#'@param xaxt - x axis text
#'@param yaxt - y axis text
#'@param xlab - x axis label
#'@param ylab - y axis label
#'
#'@return value used to scale the plot
#'
#'@details
#' if z is a vector, x must be a vector of length(z)
#' if z is a matrix, 
#'    x can be a matrix of dim(z) or
#'    x can be a vector of length ncol(z)
#' if z is a vector, y must be a vector of length(z)
#' if z is a matrix, 
#'    y can be a matrix of dim(z) or
#'    y can be a vector of length nrow(z)
#'
#'@export
#'@import graphics
#'
plotCompsAsCircles<-function(z=NULL,
                             x=NULL,
                             y=NULL,                             
                             transform=TRUE,
                             overplot=FALSE,
                             maxRadius=0.5,
                             scale=NULL,
                             fg=c("black","black"),
                             bg=c("green","blue"),
                             lty=c("solid","solid"),
                             transparency=0.5,
                             plotCohorts=FALSE,
                             units="data units",
                             main=NA,
                             subtitle=NA,
                             xlims=NULL,
                             ylims=NULL,
                             xaxt=TRUE,
                             yaxt=TRUE,
                             xlab=NA,
                             ylab=NA,
                               ...) {
  
    if (xaxt) {xaxt<-"s";} else {xaxt<-"n"}
    if (yaxt) {yaxt<-"s";} else {yaxt<-"n"}
    
    #reshape x,y,z as vectors of correct lengths
    if (is.vector(z)){
        zlen<-length(z);
        if ((length(x)!=zlen)||(length(y)!=zlen)){
            cat("If z is a vector, x and y must also be vectors of the same length!\n");
            cat("Exiting plotCompsAsCircles(...)\n");
            return(NULL);
        }
    } else if (is.matrix(z)){
        cat("z is a matrix\n");
        zdim<-dim(z);
        print(zdim);
        z<-as.vector(z);#appends by columns, not rows, so row index (y) cycles fastest
        if (is.matrix(x)) {
            cat("x is a matrix\n");
            xdim<-dim(x); 
            if (!all(xdim==zdim)){
                cat("If x and z are matrices, dim(x) and dim(z) must agree!\n")
                cat("Exiting plotCompsAsCircles(...)\n");
                return(NULL);
            }
            x<-as.vector(x);#appends by columns, not rows
        } else if (is.vector(x)){
            cat("x is a vector of length ",length(x),"\n");
            if (length(x)==zdim[2]){
                #expand x by repeating each element of it nrow(z) times
                x<-rep(x,each=zdim[1]);
            } else {
              cat("x dimensions do not agree!\n");
              cat("dim(z) = ",zdim,"\n");
              cat("length(x) must equal dim(z)[2]\n")
              return(NULL);
            }
        } else {
            cat("x must be a vector or matrix!\n")
            return(NULL);
        }
        if (is.matrix(y)) {
            cat("y is a matrix\n");
            ydim<-dim(y); 
            if (!all(ydim==zdim)){
                cat("If y and z are matrices, dim(y) and dim(z) must agree!")
                cat("Exiting plotCompsAsCircles(...)\n");
                return(NULL);
            }
            y<-as.vector(y);#appends by columns, not rows
        } else if (is.vector(y)){
            cat("y is a vector of length ",length(y),"\n");
            if (length(y)==zdim[1]){
                #expand y by repeating it as a vector ncol(z) times
                y<-rep(y,times=zdim[2]);
            } else {
              cat("y dimensions do not agree!\n");
              cat("dim(z) = ",zdim,"\n");
              cat("length(y) must equal dim(z)[1]\n")
              return(NULL);
            }
        } else {
            cat("y must be a vector or matrix!\n")
            return(NULL);
        }
    }
                             
    #transform and scale magnitude (z) data to radius of circle
    z[!is.finite(z)]<-NA;
    if (is.null(scale)) scale<-max(abs(z),na.rm=TRUE);
    cat("scale = ",scale,"\n");
    if (transform) {
        r<-sign(z)*sqrt(abs(z)/scale);
    } else {
        r<-z/scale;
    }
    
 #   print(r);
    #now order circles so they plot largest first
#     idx<-order(abs(r),decreasing=TRUE);
#     r<-r[idx];
#     x<-x[idx];
#     y<-y[idx];
    r[(r==0)]<-NA;       #set zeros to NAs so they won't plot as circles
    r[!is.finite(r)]<-NA;#set infinities to NAs so they won't plot as circles
#     print(r);
#     print(x);
#     print(y);
    
    #determine plot limits    
    if (is.null(xlims)) xlims<-range(x);
    if (is.null(ylims)) ylims<-range(y);
#    print(xlims);
#    print(ylims);
#    print(range(r[!is.na(r)]));
#    print(rbind(x,y,r));
    
    #set plot labels
    if (is.na(ylab)) ylab<-'y';
    if (is.na(xlab)) xlab<-'x';
    if (!overplot) {
        plot(xlims,ylims,type='n',xlab=xlab,ylab=ylab,xaxt=xaxt,yaxt=yaxt);
        title(main=main,line=2,adj=0);
        mtext(subtitle,side=3,adj=0,cex=0.8)
    }
    #plot positive values as symbols
    if (any(r>0,nr.rm=TRUE)){
        bgc<-rgb(t(col2rgb(bg[1])),alpha=255*transparency,maxColorValue=255);
        crcs<-r[r>0];
        if (!is.infinite(max(crcs,na.rm=TRUE))){
            cat("Positive scale = ",maxRadius*max(crcs,na.rm=TRUE),"\n")
            symbols(x[r>0],y[r>0],circles=maxRadius*crcs,add=TRUE,inches=FALSE,fg=fg[1],bg=bgc,lty=lty[1]);
        }
    }
        
    #plot negative values as symbols
    if (any(r<0,na.rm=TRUE)){
        bgc<-rgb(t(col2rgb(bg[2])),alpha=255*transparency,maxColorValue=255);
        crcs<-abs(r[r<0]);
        if (length(crcs)>0){
            cat("Negative scale = ",maxRadius*max(crcs,na.rm=TRUE),"\n")
            symbols(x[r<0],y[r<0],circles=maxRadius*crcs,add=TRUE,inches=FALSE,fg=fg[2],bg=bgc,lty=lty[2],lwd=2);
        }
    }
    
    #plot zero values (now NAs) as points
    if (any(is.na(r))){
        points(x[is.na(r)],y[is.na(r)],pch='.');
    }
        
    if (is.numeric(plotCohorts)||plotCohorts){
        if (is.logical(plotCohorts)) plotCohorts<-2;
        minAge<-ylims[1];
        maxAge<-ylims[2];
        minYr <-xlims[1];
        maxYr <-xlims[2];
        yrClasses<-seq((minYr-maxAge),(maxYr-minAge),by=plotCohorts);
        for (yrClass in yrClasses){
#            print(yrClass);
#            if ((yrClass+minAge)<maxYr){
                xc<-(yrClass+minAge):maxYr;
                yc<-minAge+0:(length(xc)-1);
#                print(xc);
#                print(yc);
                lines(xc,yc,lty="dotted");
#            }
        }
    }
        
    str<-paste("scale = ",scale,sep='');
    mtext(str,side=3,adj=1,cex=0.8)
    
    return(scale);
}

