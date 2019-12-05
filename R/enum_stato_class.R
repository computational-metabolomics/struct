#' enum_stato class
#'
#' A base class in the \pkg{struct} package. Should not be called directly.
#'
#' Extends the \code{enum} class to include stato functionality.
#' @seealso Refer to \code{\link{enum}} and \code{\link{stato}} for further
#' info.
#'
#' @export enum_stato
#' @include generics.R struct_class.R entity_class.R enum_class.R stato_class.R
#' @examples
#' E = enum_stato()
enum_stato<-setClass(
    "enum_stato",
    contains = c('enum','stato')
)


setMethod(f = 'show',
    signature = c('enum_stato'),
    definition = function(object) {
        callNextMethod()
        
        # add the stato part
        show(stato())
        
        cat('\n')
    }
)