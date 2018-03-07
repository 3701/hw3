#' Keeps only the variables you mention from your input data
#' @description 
#' This function keeps only the variables you mention
#'
#' @param .data Your input data 
#' @param ... One or more unquoted expressions separated by commas. You can treat variable names like they are positions.Positive values select variables; negative values to drop variables. If the first expression is negative, hselect() will automatically start with all variables. Use named arguments to rename selected variables. 
#' @return An object of the same class as .data.
#' @export
hselect<-function(...)
{
  f = match.fun(dplyr::select)
  f(...)
}


