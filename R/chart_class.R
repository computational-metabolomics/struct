#' chart_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export chart
#' @include generics.R struct_class.R parameter_class.R

chart<-setClass(
  "chart",
  contains=c('struct_class','parameter_class')
)

#' plot chart
#'
#' plots a chart object
#' @param obj a chart object
#' @export
setMethod(f="chart.plot",
          signature="chart",
          definition=function(obj)
          {
            return(obj)
          }
)


