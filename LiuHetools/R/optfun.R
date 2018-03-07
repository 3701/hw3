#'optfun
#'  @examples
#' optfun(x$x1,ga,c(0,3))
#' optfun(x$x1,cau,c(-10,10))
#' optfun(x$x1,bin,c(-100,0))
#' @export

ga = function(theta, x) dgamma(x, shape = theta, log = TRUE)
cau = function(theta, x) dcauchy(x, location = theta, log = TRUE)
bin = function(theta, x) dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE)

optfun = function(X,density,interval)
{
 
  log1 = function(theta,x=X)
  {
    sum(density(theta,x))
  }
  oout <- optimize(log1, maximum = TRUE, interval)
  
  return(oout$maximum)
}


