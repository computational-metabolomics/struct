#' entity_stato class
#'
#' A base class in the \pkg{struct} package. Should not be called directly.
#'
#' Extends the \code{entity} class to include stato functionality.
#' @seealso Refer to \code{\link{entity}} and \code{\link{stato}} for further
#' info.
#'
#' @export entity_stato
#' @include generics.R struct_class.R entity_class.R stato_class.R
#' @examples
#' E = entity_stato()
#' @param ... named slots and their values.
#' @return an entity_stato object
entity_stato = function(...) {
    # new object
    out = .entity_stato()
    # initialise
    out = .initialize_entity(out,...)
    return(out)
}

.entity_stato<-setClass(
    "entity_stato",
    contains = c('entity','stato')
)


setMethod(f = 'show',
    signature = c('entity_stato'),
    definition = function(object) {
        callNextMethod()
        
        # add the stato part
        show(stato())
        
        cat('\n')
    }
)