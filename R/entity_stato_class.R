#' entity_stato class
#'
#' A base class in the \pkg{struct} package. Should not be called directly.
#'
#' Extends the \code{entity} class to include stato functionality.
#' @seealso Refer to \code{\link{entity}} and \code{\link{stato}} for further
#' info.
#'
#' @export
#' @include generics.R struct_class.R entity_class.R stato_class.R
#' @examples
#' E = entity_stato(
#'     name = 'example',
#'     description = 'this is an example',
#'     type = 'numeric',
#'     value = 1,
#'     stato_id='XYZ000001'
#' )
#' @param stato_id The STATO ID for the entity
#' @inheritParams entity
#' @return an entity_stato object
entity_stato = function(name, description=character(0), type='character', 
    value=character(0),max_length=Inf,stato_id) {
    # new object
    out = .entity_stato(
        name=name, 
        description=description, 
        type=type, 
        value=value,
        max_length=max_length,
        stato_id=stato_id
    )
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
        show(stato(stato_id=object@stato_id))
        
        cat('\n')
    }
)