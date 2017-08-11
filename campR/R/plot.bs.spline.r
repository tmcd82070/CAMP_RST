#' @export
#' 
#' @title plot.bs.spline
#' 
#' @description Plot the results of fitting a temporal b-spline.  
#' 
#' @param X The basis b-spline matrix resulting from a call to function
#'   \code{bs}.
#' @param fit The generalized linear model resulting from a call to function
#'   \code{glm}.
#' @param beg.x The POSIX-formatted start date to use for plotting.
#' @param end.x The POSIX-formatted end date to use for plotting.
#'   
#' @return A plot of the fitted cubic spline, its originating data points, and
#'   the knots utilized to achive the fit.
#'   
#' @details Function \code{plot.bs.spline} simply organizes all the pieces 
#'   necessary to graph the prediction cubic piecewise polynomial resulting from
#'   the use of a b-spline.  It plots not only the (necessarily) smooth spline,
#'   but also the original points used to estimate it.  It also plots all knots,
#'   i.e., both boundary and interior.  It calculates the prediction via matrix 
#'   multiplication of the provided matrix basis \eqn{X} and the vector of 
#'   parameter \eqn{\beta} coefficients from object \code{fit}.
#'   
#'   This function is customized for use with the CAMP project, and will not 
#'   work for splines derived from data originating elsewhere without 
#'   modification.
#'   
#' @seealso \code{F.efficiency.model.enh}
#' 
#' @author WEST Inc. 
#' 
#' @examples
#' \dontrun{
#' #   ---- Plot results from an efficiency model.  Note that no parameter
#' #   ---- is provided for argument bd2 (batchDate2).   
#' plot.bs.spline(X,fit,beg.x,end.x,tmp.df)
#' }
plot.bs.spline <- function(X,fit,beg.x,end.x,eff=tmp.df,bd2=df$batchDate2[eff.ind.inside]){
  
  # X <- bspl
  # fit <- fits[[trap]]#fit
  # beg.x <- bsplBegDt
  # end.x <- bsplEndDt
  # eff <- tmp.df
  # bd2=df$batchDate2[eff.ind.inside]
  
  #   ---- For CAMP work, we assume there is no intercept.  Check for this.
  int <- attr(X,"intercept")
  if(int != FALSE){
    stop(paste0("ERROR:  This function assumes no intercept in bs object ",deparse(substitute(X)),".\n"))
  }
  Boundary.knots <- attr(X,"Boundary.knots")
  #degree <- attr(X,"degree")   # do i need this?  we always use cubic.
  knots <- attr(X,"knots")
  
  #   ---- Identify if the bspl object is really just a vector of 1s.
  intOnly <- sum(X == rep(1,nrow(X))) == nrow(X) 
  
  #   ---- In CAMP work, and in general, the X (bspl) matrix may not be sorted  
  #   ---- by row.  This means the x-variable against which we ultimately wnat to plot.  
  #   ---- For CAMP, the reduction of multiple years of data to the "one 1969/1979 year"
  #   ---- means that the bs basis matrix is, in general, not sorted.  This also means
  #   ---- that in general, an x-value could be in X twice+, via two+ rows.  This 
  #   ---- duplication in the x doesn't affect estimation, but we need to ID which rows
  #   ---- are which; i.e., we need to sort.  So, use the hard-coded batchDate object. 
  DF <- data.frame(batchDate2=bd2,X)
  b <- coef(fit)
  
  #   ---- Check if X is just a vector of 1s; i.e., an intercept-only model.  
  if( intOnly == TRUE ){
    y <- X %*% b
  } else {
    y <- cbind(rep(1,nrow(X)),X) %*% b
  }
  p <- 1/(1 + exp(-1*y))
  DF <- cbind(DF,y=y,p=p)
  
  #   ---- Get the y-coordinates for the 2 boundary knots.
  if( intOnly == TRUE){
    yboundary <- DF$p[1:2]
    yknots <- numeric(0)
  } else {
    yboundary <- 1/(1 + exp(-1*cbind(rep(1,2),predict(bspl,Boundary.knots)) %*% b))
  
    if(length(knots) > 0){
      yknots <- 1/(1 + exp(-1*cbind(rep(1,length(knots)),predict(bspl,knots)) %*% b))
    }
  }
  
  #   ---- Sort DF after multiplication by b vector.  
  DF <- DF[order(DF$batchDate2),]
  
  #   ---- Plot.
  title <- paste0("Enhanced Efficiency Cubic Spline Trap ",fit$data$TrapPositionID[1])
  plot(DF$batchDate2,DF$p,xlim=as.numeric(c(beg.x,end.x)),ylim=c(0,1),xlab="Time",ylab="Efficiency",type="l",col="black",main=title)
  par(new=TRUE)
  points(Boundary.knots,yboundary,pch=19,col="blue")
  points(tmp.df$batchDate2,tmp.df$efficiency,pch=19,col="red",cex=(fit$data$nReleased)/mean((fit$data$nReleased))  )
  if(length(knots) > 0){
    points(knots,yknots,pch=19,col="blue")
  }
  
}