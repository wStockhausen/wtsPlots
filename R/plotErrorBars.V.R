#'
#'@title Add vertical error bars to current plot
#'
#'@description Adds vertical error bars to current plot.
#'
#'@param x - vector of x coordinates or matrix w/ columns 1 & 2 taken as x,y
#'@param y - vector of y coordinates (or NULL, if x is a matrix)
#'@param upper - upper CI for y
#'@param lower - lower CI for y 
#'@param sigma - standard deviations for y
#'@param cv - cvs for y, instead of sigma
#'@param CI - upper confidence interval (if sigma or cv specified)
#'@param lognormal - flag (T/F). if true, assumed distribution is lognormal
#'@param width - relative width of error bars
#'@param ... - graphics parameters for segments (e.g., col, lty, lwd)
#'
#'@export
#'@import graphics
#'@import stats
#'
plotErrorBars.V<-function(x,
                          y=NULL,
                          upper=NULL,       #upper CI for y
                          lower=NULL,       #lower  CI for y
                          sigma=NULL,       #standard deviations for y
                          cv=NULL,          #cvs for y, instead of sigma
                          CI=0.8,           #upper confidence interval (if sigma or cv specified)
                          lognormal=FALSE,  #if true, assumed distribution is lognormal
                          width=1,          #relative width of error bars
                          ...               #graphics parameters for segments (e.g., col, lty, lwd)
                          ) {
#    old.par<-par(new=T);
    if (is.null(y)) {
        if (is.vector(x)) {
            yp<-x;
            xp<-1:length(x);
        } else if (is.matrix(x)) {
            yp<-x[,2];
            xp<-x[,1];
        } else {
            cat('Exiting--not implemented for this type of x. \n');
            return;
        }
    } else {
        xp = x;
        yp = y;
    }
    if (!is.null(cv)){
        sigma<-cv*yp;
    }
    if (!is.null(sigma)) {
        if (!lognormal) {
            upper<-qnorm(1-(1-CI)/2,sd=sigma);#upper confidence bounds
            lower<-qnorm((1-CI)/2,sd=sigma);  #lower confidence bounds
        } else {
            lnyp<-log(yp);
            lnsd<-sqrt(log(1+(sigma/yp)^2))
            upper<-qlnorm(1-(1-CI)/2,meanlog=lnyp,sdlog=lnsd)-yp;
            lower<-qlnorm((1-CI)/2,meanlog=lnyp,sdlog=lnsd)-yp;        
        }
    }
    usr<-par("usr");
#    cat("usr = ",usr,"\n")
    ebw<-width*(usr[2]-usr[1])/((max(xp,na.rm=TRUE)-min(xp,na.rm=TRUE)));
#    cat("ebw = ",ebw,"\n");
    for (i in 1:length(xp)) {
        if (!is.null(upper)) {
            segments(xp[i],yp[i],xp[i],yp[i]+upper[i],...);
            segments(xp[i]-ebw,yp[i]+upper[i],xp[i]+ebw,yp[i]+upper[i],...);
        }
        if (!is.null(lower)) {
            segments(xp[i],yp[i],xp[i],yp[i]+lower[i],...);
            segments(xp[i]-ebw,yp[i]+lower[i],xp[i]+ebw,yp[i]+lower[i],...);
        }
    }
#    on.exit(par(old.par));
}