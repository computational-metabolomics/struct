#' stato_model_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export model.stato
#' @include generics.R struct_class.R entity_class.R stato_class.R

model.stato<-setClass(
  "model.stato",
  contains=c('model','stato')
)
