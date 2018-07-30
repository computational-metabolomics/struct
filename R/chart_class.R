#' chart_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export chart
#' @include generics.R struct_class.R parameter_class.R

chart<-setClass(
  "chart",
  contains=c('struct_class','parameter_class')
)

setMethod(f="chart.plot",
          signature="chart",
          definition=function(obj)
          {
            return(obj)
          }
)

#' @export
setMethod(f="show",
          signature=c("chart"),
          definition=function(object)
          {
            cat('Name: ', name(object),'\nDescription: ',description(object),'\nType:',type(object))
          }
)

