#' enum_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export enum
#' @include generics.R struct_class.R
#' #' @examples
#' E = enum()
#'
enum<-setClass(
    "enum",
    slots=c('list'),
    contains='entity',
    prototype=list(name='name not provided',
        description='no description provided')
)

setMethod(f="initialize",
    signature="enum",
    definition=function(.Object,...)
    {
        L=list(...)
        N=names(L)

        for (i in N) {
            slot(.Object,i)=L[[i]]
        }
        if (!is.null(.Object@list) & is.null(.Object@value)) {
            value(.Object)=.Object@list[1]
        }
        return(.Object)
    }
)

#' set the value of an enum object
#'
#' sets the value for an enum object
#' @param obj an enum object
#' @param value a valid value for the enum
#' @export
#' @examples
#' E = enum(list=c('hello','world'))
#' value(E) = 'hello' # value MUST be in the enum list
#'
setMethod(f="value<-",
    signature=c("enum"),
    definition=function(obj,value)
    {
        if (value %in% obj@list) {
            obj@value=value
        } else {
            stop(paste0(value,' is not a valid choice for this enum.'))
        }
        return(obj)
    }
)


