#' stato_entity_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export entity.stato
#' @include generics.R struct_class.R entity_class.R stato_class.R
#' #' @examples
#' E = entity.stato()

entity.stato<-setClass(
    "entity.stato",
    contains=c('entity','stato')
)


