#' iterator class
#'
#' A class for iterative approaches that involve the training/prediction of a model multiple times.
#' not intended to be called directly, this class should be inherited to provide functionality for method-specific classes.
#' @export iterator
#' @inheritParams run
#' @param ML a model sequence object
#' @param M a model object
#' @param value value
#' @param e1 an iterator object
#' @param e2 an iterator or a model object
#' @include generics.R  parameter_class.R output_class.R model_class.R metric_class.R model_list_class.R
#' @examples
#' I = iterator()
#' I = iterator() * model()

iterator<-setClass(
  "iterator",
  contains = c('struct_class','parameter_class','outputs_class'),
  slots=c(type='character',
          models='ANY', # any here, but types enforced by e.g. resampler
          result='character')
)


#' @describeIn iterator run a model/model.seq mutliple times for the input data
#' @export
#' @examples
#' D = dataset()
#' MET = metric()
#' I = iterator() * model()
#' I = run(I,D,MET)
#'
setMethod(f="run",
          signature=c("iterator","dataset",'metric'),
          definition=function(I,D,MET=NULL)
          {
            warning('the base iterator function was called, not the one defined for your specific iterator')
            return(I)
          }
)


#' @describeIn iterator evaluate the performance of a model/model.seq  using the input metric
#' @export
setMethod(f="evaluate",
          signature=c("iterator","metric"),
          definition=function(I,MET)
          {
            return(I)
          }
)

#' @describeIn iterator get the model/model.seq for an iterator object
#' @export
setMethod(f="models",
          signature=c("iterator"),
          definition=function(ML)
          {
            return(ML@models)
          }
)

setClassUnion("model_OR_iterator", c("model", "iterator","model.seq"))

#' @describeIn iterator set the model/model.seq to be run for multiple iterations
#' @export
setMethod(f="models<-",
          signature=c("iterator",'model_OR_iterator'),
          definition=function(ML,value)
          {
            ML@models=value
            return(ML)
          }
)

#' @describeIn iterator set the model/model.seq to be run for multiple iterations
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

#' @describeIn iterator combine an interator with other model or interator objects
#' @export
setMethod(f='*',
          signature=c(e1='iterator',e2='model_OR_iterator'),
          definition=function(e1,e2)
          {
            m=models(e1)
            if (is(m,'iterator')) {
              models(e1)=m*e2
            } else {
              #print(paste('putting',class(e2),'into',class(e1),sep=' '))
              models(e1)=e2
            }
            return(e1)
          }
)
