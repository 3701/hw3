#' fhw15
#' @description 
#' Calculate the mu and variance with data weighted with probability sums to 1; with GIEMO
#'
#' 
#'
#' @param x array of data
#' @param p array of probability
#' @return mean, variance of input data
#' @export

fhw15<-function(x,p)
{
  

GIEMO <- function(xdog,pdog)
{
  stopifnot(is.numeric(pdog))
  stopifnot(is.finite(pdog))
  stopifnot(length(pdog)>0)
  
  stopifnot(!is.nan(pdog))
  stopifnot(!is.na(pdog))
  
  stopifnot(all.equal(sum(pdog),1))
  stopifnot(is.numeric(xdog))
  stopifnot(is.finite(xdog))
  stopifnot(!is.na(xdog))
  stopifnot(!is.nan(xdog))
  stopifnot(length(xdog)>0)
}
GIEMO(x,p)

mu2<-sum(x[1:30]*p[1:30])
var1 = 0

for(i in 1:30)
{
  var1 = var1 + p[i]*(x[i]-mu2)^2
}
c(mu2,var1)

}