#' fquiz23
#' @description 
#'standardizes matrix's columns; with GIEMO
#'
#' 
#'
#' @param x Name of your data

#' @return standardized matrix columns
#' @export

fquiz23<-function(x)
{
GIEMO3<-function(i) {
  stopifnot(is.matrix(i))
  stopifnot(nrow(i)>1)
  stopifnot(length(i)>0)
  stopifnot(is.numeric(i))
  stopifnot(is.finite(i))
}

GIEMO3(x)


(x[,1:4]-apply(x,2,mean))/apply(x,2,sd)
}
