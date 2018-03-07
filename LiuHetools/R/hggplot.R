#' hggplot
#' @description 
#'Scatterplot your data, using the same syntax as ggplot2::ggplot() 
#'
#' 
#'
#' @param data Name of your data
#' @param mapping  Default list of aesthetic mappings to use for plot. If not specified, must be suppled in each layer added to the plot.
#' @param environment an variable defined in the aesthetic mapping is not found in the data, this function will look for it in this environment. 
#' @return Scatterplot of you data with your specified x and y and other value.
#' @export


hggplot<-function(...)
  {
  g = match.fun(ggplot2::ggplot)
  g(...)+geom_point()
}

