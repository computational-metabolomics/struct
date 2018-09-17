#' dataset class
#'
#' an object for holding raw data and associated meta data
#' @export dataset
#' @slot name (character) name of the dataset
#' @slot description (character) brief description of the dataset
#' @slot data (data.frame) matrix of data, samples in rows, variables/features in columns.
#' @slot sample.meta (data.frame) data frame of sample meta data e.g. group membership
#' @slot variable.meta (data.frame) data frame of variable meta data
#' @rdname dataset
#' @include generics.R struct_class.R stato_class.R chart_class.R chart_stato.R
#'
dataset<-setClass(
  "dataset",
  contains=c("struct_class"),
  slots=c(name="character",
          description="character",
          data="data.frame",
          sample_meta="data.frame",
          variable_meta="data.frame"
  ),
  prototype=list(name="Dataset000",
                 description="an empty dataset object"
  )

)

#' @export
#' @param obj a dataset object
#' @rdname dataset
setMethod(f="dataset.data",
          signature=c("dataset"),
          definition=function(obj)
          {
            return(obj@data)
          }
)

#' @export
#' @rdname dataset
setMethod(f="dataset.data<-",
          signature=c("dataset"),
          definition=function(obj,value)
          {
            obj@data=value
            return(obj)
          }
)

#' @export
#' @rdname dataset
setMethod(f="dataset.sample_meta",
          signature=c("dataset"),
          definition=function(obj)
          {
            return(obj@sample_meta)
          }
)

#' @export
#' @rdname dataset
setMethod(f="dataset.sample_meta<-",
          signature=c("dataset"),
          definition=function(obj,value)
          {
            obj@sample_meta=value
            return(obj)
          }
)

#' @export
#' @rdname dataset
setMethod(f="dataset.variable_meta",
          signature=c("dataset"),
          definition=function(obj)
          {
            return(obj@variable_meta)
          }
)

#' @export
#' @rdname dataset
setMethod(f="dataset.variable_meta<-",
          signature=c("dataset"),
          definition=function(obj,value)
          {
            obj@variable_meta=value
            return(obj)
          }
)




