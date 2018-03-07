#' fquiz22
#' @description 
#' fquiz22 invoked as: a %function% x; a  is numeric matrix  and x is numeric vector. fquiz22 calculates x^T A^{-1} x; With GIEMO 
#'
#' 
#'
#' @param a numeric matrix
#' @param x  numeric vector
#' @return value of  x^T A^{-1} x
#' @export

"%fquiz22%"<-function(a,x)
{
x<-as.matrix(x)

GIEMO<-function(o,b) {
  stopifnot(is.numeric(o))
  stopifnot(is.finite(o))
  stopifnot(length(o)>0)
  stopifnot(length(b)>0)
  stopifnot(is.numeric(b))
  stopifnot(is.finite(b))
}

GIEMO(x,a)

alice<-function(a,x){
  return(t(x)%*%solve(a)%*%x)
}
alice(a, x)
}