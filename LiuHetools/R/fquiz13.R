#' fquiz13
#' @description 
#'Using dgamma calculte maximum likelihood estimate of input data
#'
#' @param x Name of your data
#' @return maximum likelihood estimate of input data Using dgmma
#' @export



fquiz13<- function(x)
{
alpha = pi
logl <- function(x)
  sum(dgamma(length(x),shape=alpha ,  log = TRUE))
interval <- mean(x) + c(-1, 1) * 3 * sd(x)
interval <- pmax(mean(x) / 1e3, interval)
oout <- optimize(logl, maximum = TRUE, interval)
oout$maximum
}