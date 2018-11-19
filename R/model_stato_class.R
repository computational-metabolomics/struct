#' stato_model_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export model.stato
#' @include generics.R stato_class.R model_class.R
#' @examples
#' M = model.stato()

model.stato<-setClass(
    "model.stato",
    contains=c('model','stato')
)
