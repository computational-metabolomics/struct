#' metric class
#'
#' A class for metrics to assess performance of e.g. models, iterators.
#' Not intended to be called directly, this class should be inherited to provide functionality for method-specific classes.
#' @export metric
#' @include generics.R parameter_class.R output_class.R model_class.R iterator_class.R model_list_class.R
#' @inheritParams calculate

metric<-setClass(
  "metric",
  contains = c('struct_class'),
  slots=c(type='character',
          value='numeric'
  )
)

#' @describeIn metric calculate a metric
#' @export
setMethod(f="calculate",
          signature=c('metric'),
          definition=function(MET,Y,Yhat)
          {
            return(MET)
          }
)

#' @describeIn metric get the caluclated value for a metric
#' @export
setMethod(f="value",
          signature=c("metric"),
          definition=function(MET)
          {
            return(MET@value)
          }
)
