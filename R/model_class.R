#' model class
#'
#' A class for models that can be trained/applied to datasets e.g. PCA, PLS etc.
#' Also used for preprocessing steps that require application to test sets.
#' not intended to be called directly, this class should be inherited to provide functionality for method-specific classes.
#' @export model
#' @include generics.R  parameter_class.R output_class.R dataset_class.R


model<-setClass(
  "model",
  contains = c('struct_class','parameter_class','outputs_class'),
  slots=c(type='character',
          predicted='character',
          stato.id='character'
          )
  )

#' @describeIn model train the model using input data
#' @export
setMethod(f="train",
          signature=c("model","dataset"),
          definition=function(M,D)
          {
            return(M)
          }
)

#' @describeIn model apply the model to input data
#' @export
setMethod(f="predict",
          signature=c("model","dataset"),
          definition=function(M,D)
          {
            return(M)
          }
)

#' @describeIn model get prediction output from model
#' @export
setMethod(f='predicted',
          signature=c('model'),
          definition=function(M)
          {
            return(output.value(M,predicted.name(M)))
          }
)

#' @describeIn model get prediction output name for model
#' @export
setMethod(f='predicted.name',
          signature=c('model'),
          definition=function(M)
          {
            return(M@predicted)
          }
)

#' @describeIn model set prediction output from model
#' @export
setMethod(f='predicted.name<-',
          signature=c('model','character'),
          definition=function(M,value)
          {
            M@predicted=value
            return(M)
          }
)


