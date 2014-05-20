#'
#'@title Add horizontal error bars to current plot
#'
#'@description Adds horizontal error bars to current plot.
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
plotErrorBars.H<-function(x,
                        y,
                        upper=NULL,       #upper CI for x
                        lower=NULL,       #lower  CI for x
                        sigma=NULL,       #standard deviations for x
                        cv=NULL,          #cvs for x, instead of sigma
                        CI=0.95,          #upper confidence interval (if sigma or cv specified)
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
        sigma<-cv*xp;
    }
    if (!is.null(sigma)) {
        if (!lognormal) {
            upper<-qnorm(1-(1-CI)/2,sd=sigma);
            lower<-qnorm((1-CI)/2,sd=sigma);
        } else {
            lnxp<-log(xp);
            lnsd<-sqrt(log(1+(sigma/xp)^2))
            upper<-qlnorm(1-(1-CI)/2,meanlog=lnxp,sdlog=lnsd)-xp;
            lower<-qlnorm((1-CI)/2,meanlog=lnxp,sdlog=lnsd)-xp;        
        }
    }
    usr<-par("usr");
   cat("usr = ",usr,"\n")
    ebw<-width*(usr[4]-usr[3])/((max(yp,na.rm=TRUE)-min(yp,na.rm=TRUE)));
   cat("ebw = ",ebw,"\n");
    for (i in 1:length(xp)) {
        if (!is.null(upper)) {
            segments(xp[i],yp[i],xp[i]+upper[i],yp[i],...);
            segments(xp[i]+upper[i],yp[i]-ebw,xp[i]+upper[i],yp[i]+ebw,...);
        }
        if (!is.null(lower)) {
          segments(xp[i],yp[i],xp[i]+lower[i],yp[i],...);
          segments(xp[i]+lower[i],yp[i]-ebw,xp[i]+lower[i],yp[i]+ebw,...);
        }
    }
#    on.exit(par(old.par));
}