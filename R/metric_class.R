#' metric class
#'
#' A class for metrics to assess performance of e.g. models, iterators.
#' Not intended to be called directly, this class should be inherited to provide
#' functionality for method-specific classes.
#' @export metric
#' @param Y the true class labels
#' @param Yhat the predicted class labels
#' @param value value
#' @include generics.R parameter_class.R output_class.R model_class.R
#' @include iterator_class.R model_list_class.R
#' @inheritParams calculate
#' @examples
#' MET = metric()

metric<-setClass(
    "metric",
    contains = c('struct_class'),
    slots=c(type='character',
        value='numeric'
    )
)

#' @describeIn metric calculate a metric
#' @export
#' @examples
#' \dontrun{
#' M = metric()
#' calculate(M,Y,Yhat)
#' }
setMethod(f="calculate",
    signature=c('metric'),
    definition=function(MET,Y,Yhat)
    {
        warning('no calculation provided for this metric')
        return(MET)
    }
)

#' @describeIn metric get the caluclated value for a metric
#' @export
#' @examples
#' MET = metric()
#' value(MET)
setMethod(f="value",
    signature=c("metric"),
    definition=function(MET)
    {
        return(MET@value)
    }
)
