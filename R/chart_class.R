#' chart_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export chart
#' @include generics.R struct_class.R

chart<-setClass(
  "chart",
  slots=c('opt'='list',
          'fcn'='function'
          ),
  contains=c('struct_class')
)


#' @export
setMethod(f="chart.opt",
          signature=c("chart"),
          definition=function(obj)
          {
            return(obj@opt)
          }
)

#' @export
setMethod(f="chart.opt<-",
          signature=c("chart"),
          definition=function(obj,value)
          {
            obj@opt=value
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
