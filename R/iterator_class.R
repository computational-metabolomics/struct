#' iterator class
#'
#' A class for iterative approaches that ivolve the training/prediction of a model multiple times.
#' not intended to be called directly, this class should be inherited to provide functionality for method-specific classes.
#' @export iterator
#' @inheritParams run
#' @include generics.R  parameter_class.R output_class.R model_class.R metric_class.R model_list_class.R

iterator<-setClass(
  "iterator",
  contains = c('struct_class','parameter_class','outputs_class'),
  slots=c(type='character',
          models='ANY', # any here, but types enforced by e.g. resampler
          result='character')
)


#' @describeIn iterator run a model/model.list mutliple times for the input data
#' @export
setMethod(f="run",
          signature=c("iterator","dataset",'metric'),
          definition=function(I,D,MET=NULL)
          {
            stop('the base iterator function was called, not the one defined for your specific iterator')
            return(I)
          }
)


#' @describeIn iterator evaluate the performance of a model/model.list  using the input metric
#' @export
setMethod(f="evaluate",
          signature=c("iterator","metric"),
          definition=function(I,MET)
          {
            return(I)
          }
)

#' @describeIn iterator get the model/model.list for an iterator object
#' @export
setMethod(f="models",
          signature=c("iterator"),
          definition=function(ML)
          {
            return(ML@models)
          }
)

setClassUnion("model_OR_iterator", c("model", "iterator","model.list"))

#' @describeIn iterator set the model/model.list to be run for multiple iterations
#' @export
setMethod(f="models<-",
          signature=c("iterator",'model_OR_iterator'),
          definition=function(ML,value)
          {
            ML@models=value
            return(ML)
          }
)

#' @describeIn iterator set the model/model.list to be run for multiple iterations
#' @export
setMethod(f="models<-",
          signature=c("iterator",'model_OR_iterator'),
          definition=function(ML,value)
          {
            ML@models=value
            return(ML)
          }
)

#' @describeIn iterator get prediction output from iterator
#' @export
setMethod(f='result',
          signature=c('iterator'),
          definition=function(M)
          {
            return(output.value(M,result.name(M)))
          }
)

#' @describeIn iterator get prediction output name for iterator
#' @export
setMethod(f='result.name',
          signature=c('iterator'),
          definition=function(M)
          {
            return(M@result)
          }
)
