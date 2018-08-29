#' method class
#'
#' A class for methods that are applied to datasets in a single step e.g. normalisation.
#' not intended to be called directly, this class should be inherited to provide functionality for method-specific classes.
#' @export method
#' @include generics.R  parameter_class.R output_class.R dataset_class.R


method<-setClass(
  "method",
  contains = c('struct_class','parameter_class','outputs_class'),
  slots=c(type='character',
          predicted='character'
  )
)

#' @describeIn model train the model using input data
#' @export
setMethod(f="method.apply",
          signature=c("model","dataset"),
          definition=function(M,D)
          {
            return(M)
          }
)

#' @describeIn model get prediction output from model
#' @export
setMethod(f='predicted',
          signature=c('method'),
          definition=function(M)
          {
            return(output.value(M,predicted.name(M)))
          }
)

#' @describeIn model get prediction output name for model
#' @export
setMethod(f='predicted.name',
          signature=c('method'),
          definition=function(M)
          {
            return(M@predicted)
          }
)

#' @describeIn model set prediction output from model
#' @export
setMethod(f='predicted.name<-',
          signature=c('method','character'),
          definition=function(M,value)
          {
            M@predicted=value
            return(M)
          }
)


