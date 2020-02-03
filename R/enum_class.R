#' Enum objects
#'
#' A base class in the \pkg{struct} package. Not normally called directly.
#'
#' An enum object is a special type of entity object that ensures the value must
#' be one from a list of allowed values.
#'
#' Enum objects are usually defined in the prototype of another object, but
#' can be extracted using \code{param_obj} and \code{output_obj}.
#'
#' @export
#' @include entity_class.R
#' @return an enum object
#' @examples
#' # Create a new enum object
#' E = enum(
#'     name = 'example',
#'     description = 'this is an example',
#'     type = 'character',
#'     value = 'hello',
#'     allowed = c('hello','world')
#' )
#'
#' # Get/set the value of the entity object
#' value(E)
#' value(E) = 'world'
#' @param allowed A list of allowed values
#' @inheritParams entity
#' @rdname enum
enum = function(name, description=character(0), type='character', 
    value=character(0),max_length=1,allowed) {
    
    # new object
    out = .enum(
        name=name, 
        description=description,
        type=type,
        value=value,
        max_length=max_length,
        list=allowed
    )
    return(out)
}

.enum<-setClass(
    "enum",
    slots = c('list'),
    contains = 'entity',
    prototype = list(name = 'name not provided',
        description = 'no description provided'),
    validity = function(object) {
        check_list = object@value %in% object@list
        
        # check enum validity
        msg = TRUE
        if (!check_list) {
            msg = paste0(object$name,': enum value must be in enum list')
        }

        return(msg)
    }
)


#' @rdname enum
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


setMethod(f = 'show',
    signature = c('enum'),
    definition = function(object) {
        callNextMethod()
        
        cat('list:         ',paste0(object@list,collapse=', '))
        cat('\n')
    }
)
