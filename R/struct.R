#' StRUCT: Statistics in R Using Class Templates
#'
#' This package defines classes (templates) for developing statistical workflows.
#' These classes can be extended using other packages, making
#' it easier to combine methods from different packages into a robust workflow.
#' Integration with STATO: the statistical methods ontology 
#' (\url{https://www.ebi.ac.uk/ols/ontologies/stato}) provides standardised definitions
#' for many statistical methods.
#'
#' @section Classes:
#' The classes include:
#' 
#' \itemize{
#'   \item{\strong{DatasetExperiment}}{: An extension of the SummarizedExperiment object
#'   by Bioconductor}
#'   \item{\strong{model}}{: A template for training and applying statistics}
#'   \item{\strong{iterator}}{: A template for resampling, optimisation and validation of statistical models}
#'   \item{\strong{chart}}{: A template for generating graphical outputs for models and iterators}
#' }
#' @docType package
#' @name struct
NULL
## NULL
