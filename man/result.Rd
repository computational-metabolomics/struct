% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{result}
\alias{result}
\title{Iterator result}
\usage{
result(M)
}
\arguments{
\item{M}{an iterator object}
}
\value{
the returned output varies with the algorithm implemented
}
\description{
Returns the results of an iterator. This is used to control model flow in a
similar way to \code{predict} for model and model_seq objects.
}
\examples{
D = iris_DatasetExperiment() # get some data
MET = metric()  # use a metric
I = example_iterator() # initialise iterator
models(I) = example_model() # set the model
I = run(I,D,MET) # run
result(I)
}
