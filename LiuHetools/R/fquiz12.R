#' fquiz12
#' @description 
#' Return mean, variance, standard deviation of input data, with GIEMO
#'
#' 
#'
#' @param x Name of your data
#' @return  mean, variance, standard deviation of input data
#' @export

fquiz12<-function(x)
{
if (length(x) == 0 || is.na(x) ||is.nan(x)
    || !is.numeric(x) || is.infinite(x)){
  print("Error")
}else {
  mean<- (1/length(x))*sum(x[1:length(x)])
  a=0
  for (k in 1:length(x)){
    a = a + (x[k]-mean)^2}
  var<- (1/length(x))*a
  sd<- sqrt(var)
  Output <- list(mean = mean,var = var,sd = sd)
  Output}
}