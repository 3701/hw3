#' fhw15
#' @description 
#' Calculate the mu and variance with data weighted with probability sums to 1; without GIEMO
#'
#' 
#'
#' @param x array of data
#' @param p array of probability
#' @return mean, variance of input data
#' @export

fhw14<-function(x,p)
{
  
  mu2<-sum(x[1:30]*p[1:30])
  var1 = 0
  
  for(i in 1:30)
  {
    var1 = var1 + p[i]*(x[i]-mu2)^2
  }
c(mu2,var1)
}
