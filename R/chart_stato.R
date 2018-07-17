#' chart_stato_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export chart.stato
#' @include generics.R struct_class.R chart_class.R stato_class.R

chart.stato<-setClass(
  "chart.stato",
  contains=c('chart','stato')
)


