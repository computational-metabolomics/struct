#' chart_stato_class
#'
#' A base class in the \pkg{struct} package for creating chart objects with
#' STATO ids
#' @export chart.stato
#' @include generics.R struct_class.R chart_class.R stato_class.R
#' @examples
#' obj = chart.stato() # create chart object

chart.stato<-setClass(
    "chart.stato",
    contains=c('chart','stato')
)


