#' fquiz22
#' @description 
#' Loading a 3d matrix; 
#' Apply the R function median to the three two-dimensional margins (indexed by pairs of indices) 
#' and the three one-dimensional margins (indexed by single indices) of this array. 
#'
#' 
#'
#' @param x 3d matrix
#' @return median to the three two-dimensional margins (indexed by pairs of indices) 
#' and the three one-dimensional margins (indexed by single indices) of this array. 
#' @export
#' 
#' 
#' 

fhw26<-function(x)
{
  a<-apply(x,c(1,3),median)
  b<-apply(x,c(1,2),median)
  c<-apply(x,c(2,3),median)
  
  d<-apply(x,c(3),median)
  e<-apply(x,c(1),median)
  f<-apply(x,c(2),median)
  list(a,b,c,d,e,f)
}

