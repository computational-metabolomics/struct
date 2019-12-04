#' Enum objects
#'
#' A base class in the \pkg{struct} package. Not normally called directly.
#'
#' An enum object is a special type of entity object that ensures the value must
#' be one from a list of allowed values.
#'
#' Enum objects are usually defined in the prototype of another object, but
#' can be extracted using \code{param.obj} and \code{output.obj}.
#'
#' @export enum
#' @include entity_class.R
#' @return an enum object
#' @examples
#' # Create a new enum object
#' E = enum(
#'     name = 'example',
#'     description = 'this is an example',
#'     type = 'character',
#'     value = 'hello',
#'     list = c('hello','world')
#' )
#'
#' # Get/set the value of the entity object
#' value(E)
#' value(E) = 'world'
#'
enum<-setClass(
    "enum",
    slots = c('list'),
    contains = 'entity',
    prototype = list(name = 'name not provided',
        description = 'no description provided')
)

setMethod(f = "initialize",
    signature = "enum",
    definition = function(.Object,...)
    {
        L = list(...)
        N = names(L)

        for (i in N) {
            slot(.Object,i) = L[[i]]
        }
        if (!is.null(.Object@list) & is.null(.Object@value)) {
            value(.Object) = .Object@list[1]
        }
        return(.Object)
    }
)

#' @describeIn enum set the value for an enum
#' @param obj an enum object
#' @param value value of the enum
#' @export
setMethod(f = "value<-",
    signature = c("enum"),
    definition = function(obj,value) {
        if (value %in% obj@list) {
            obj@value = value
        } else {
            stop(paste0(value,' is not a valid choice for this enum.'))
        }
        return(obj)
    }
)


