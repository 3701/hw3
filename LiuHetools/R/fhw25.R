#' fhw25
#' @description 
#'  a function just like the function array in the R base package, but is simpler.
#'
#'
#' @param  X is a matrix of any type a matrix can have (numeric, character, logical, complex)
#' @param MARGIN is either (the number) 1 or (the number) 
#' @param  FUN is an R function that maps vectors to vectors
#' @param  ... is passed to FUN, that is, any arguments to myapply that do not match X, MARGIN, or
#'  FUN are passed to FUN whenever it is called by fhw25
#'  
#' @return result of the matrix calculation using FUN
#' @export
#' 
#' 
#' 

fhw25<- function(X,MARGIN,FUN, ...)
{
  #stopifnot(length(dim(X))==2)
  
  if (length(dim(X))!=2)
  {
    stop("MATRIX IS NOT 2D")
  }
  if( !(MARGIN%in% c(1,2)))
  {
    stop("MARGIN not in 1 ,2")
  }
  
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)
  
  if (MARGIN == 1)
  {
    
    result = list()
    for (i in 1:R)
    {
      result[i] = f(X[i,],...)
    }
  } else if (MARGIN == 2)
  {
    result = list()
    for(j in 1:C)
    {
      result[j] = f(X[,j],...)
    }
    
  }
  return(simplify2array(result)) 
}
