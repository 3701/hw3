% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hselect.R
\name{hselect}
\alias{hselect}
\title{Keeps only the variables you mention from your input data}
\usage{
hselect(...)
}
\arguments{
\item{...}{One or more unquoted expressions separated by commas. You can treat variable names like they are positions.Positive values select variables; negative values to drop variables. If the first expression is negative, hselect() will automatically start with all variables. Use named arguments to rename selected variables.}

\item{.data}{Your input data}
}
\value{
An object of the same class as .data.
}
\description{
This function keeps only the variables you mention
}
