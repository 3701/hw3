#' fquiz11
#' @description 
#'Calculate the mean, variance, standard deviation of input data
#'
#' 
#'
#' @param x Name of your data
#' @return mean, variance, standard deviation of input data
#' @export


fquiz11<-function(x)
{
mean<- (1/length(x))*sum(x[1:length(x)])
a=0
for (k in 1:length(x)){
  a = a + (x[k]-mean)^2}
var<- (1/length(x))*a
sd<- sqrt(var)
Output <- list(mean = mean,var = var,sd = sd)
Output
}


