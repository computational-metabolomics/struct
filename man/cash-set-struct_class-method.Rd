% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/struct_class.R
\name{$<-,struct_class-method}
\alias{$<-,struct_class-method}
\title{Get/set parameter or output values}
\usage{
\S4method{$}{struct_class}(x, name) <- value
}
\arguments{
\item{x}{An object derived from struct_class}

\item{name}{The name of the slot to access}

\item{value}{The value to assign}
}
\value{
Parameter/output value
}
\description{
Dollar syntax can be used to as a shortcut for getting/setting input parameter
and output values for struct objects.
}
\examples{
M = example_model()
M$value_1 = 10
M$value_1 # 10
}
