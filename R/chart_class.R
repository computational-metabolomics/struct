#' chart_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export chart
#' @include generics.R struct_class.R parameter_class.R
#' @rdname charts
#' @examples
#' obj = chart() # create chart object
chart<-setClass(
    "chart",
    contains=c('struct_class','parameter_class')
)

#' plot chart
#'
#' plots a chart object
#' @param obj a chart object
#' @rdname charts
#' @export
#' @examples
#' chart.plot(obj) # plot chart
setMethod(f="chart.plot",
    signature="chart",
    definition=function(obj)
    {
        warning('no chart defined')
        return(obj)
    }
)


