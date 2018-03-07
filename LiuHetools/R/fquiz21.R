#' fquiz21
#' @description 
#' Give a numeric matrix A and a numeric vector x, calculates x^T A^{-1} x;with GIEMO
#'
#' 
#'
#' @param a numeric matrix A 
#' @param x numeric vector x
#' @return value of x^T A^{-1} x
#' @export 


fquiz21<-function(a,x)
{
  GIEMO<-function(o,b) {
    
    stopifnot(dim(o)[1] ==dim(b)[1])
    stopifnot(is.numeric(o))
    stopifnot(is.finite(o))
    stopifnot(length(o)>0)
    stopifnot(length(b)>0)
    stopifnot(is.numeric(b))
    stopifnot(is.finite(b))
  }
  
GIEMO(x,a)
t(x)%*%solve(a)%*%x
}
