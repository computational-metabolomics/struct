#' stato_method_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export method.stato
#' @include generics.R stato_class.R method_class.R
#' @examples
#' M = method.stato()

method.stato<-setClass(
  "method.stato",
  contains=c('method','stato')
)
