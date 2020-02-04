#' enum_stato class
#'
#' A base class in the \pkg{struct} package. Should not be called directly.
#'
#' Extends the \code{enum} class to include stato functionality.
#' @seealso Refer to \code{\link{enum}} and \code{\link{stato}} for further
#' info.
#'
#' @export
#' @include generics.R struct_class.R entity_class.R enum_class.R stato_class.R
#' @examples
#' E = enum_stato(
#'     name='example',
#'     allowed=list('choice_1','choice_2'),
#'     value='choice_1',
#'     type='character',
#'     stato_id='XYZ000001'
#' )
#' @inheritParams entity_stato
#' @inheritParams enum
#' @rdname enum_stato
#' @return an enum_stato object
enum_stato = function(name, description=character(0), type='character', 
    value=character(0),max_length=1,allowed,stato_id) {
    
    # new object
    out = .enum_stato(
        name=name, 
        description=description,
        type=type,
        value=value,
        max_length=max_length,
        allowed=allowed,
        stato_id=stato_id
    )
    return(out)
}

.enum_stato<-setClass(
    "enum_stato",
    contains = c('enum','stato')
)


setMethod(f = 'show',
    signature = c('enum_stato'),
    definition = function(object) {
        callNextMethod()
        
        # add the stato part
        show(stato(stato_id=object@stato_id))
        
        cat('\n')
    }
)