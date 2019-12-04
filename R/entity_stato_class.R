#' entity.stato class
#'
#' A base class in the \pkg{struct} package. Should not be called directly.
#'
#' Extends the \code{entity} class to include stato functionality.
#' @seealso Refer to \code{\link{entity}} and \code{\link{stato}} for further
#' info.
#'
#' @export entity.stato
#' @include generics.R struct_class.R entity_class.R stato_class.R
#' @examples
#' E = entity.stato()
entity.stato<-setClass(
    "entity.stato",
    contains = c('entity','stato')
)

#' @export
setMethod(f = 'show',
    signature = c('entity.stato'),
    definition = function(object) {
        callNextMethod()

        # add the stato part
        show(stato())
        
        cat('\n')
    }
)