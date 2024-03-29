% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/parameter_class.R
\name{is_param}
\alias{is_param}
\alias{is_param,struct_class-method}
\title{Verify parameter}
\usage{
is_param(obj, name)

\S4method{is_param}{struct_class}(obj, name)
}
\arguments{
\item{obj}{An object derived from struct_class}

\item{name}{Name of parameter}
}
\value{
TRUE if parameter name is valid, FALSE if not
}
\description{
Verify that the input name is a valid input parameter for an object
}
\examples{
M = example_model()
is_param(M,'value_1') # TRUE
is_param(M,'alpha')   # FALSE
}
