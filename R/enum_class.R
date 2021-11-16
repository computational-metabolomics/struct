#' Enum objects
#'
#' A base class in the \pkg{struct} package. Not normally called directly.
#'
#' An enum object is a special type of entity object that ensures the value must
#' be one from a list of allowed values.
#' 
#' Enum object also contain a "return" slot, that allows "value" to be used as a
#' means of indexing return values e.g. setting a value of 1 might return 'A'.
#' By default the return slot is equal to the allowed slot. Slot "selected"
#' indicates which of the allowed values was chosen to obtaint he return value.
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
#'     allowed = c('hello','world'),
#' )
#'
#' # Get/set the value of the enum object
#' value(E)
#' value(E) = 'world'
#' 
#' # create an enum that returns based on index matching
#' E = enum(
#'     name = 'example',
#'     description = 'this is an example',
#'     type = 'character',
#'     value = 'one',
#'     allowed = c(1,2,3),
#'     return = c('one','two','three')
#' )
#' 
#' # calling value = ... searches the "allowed" slot for a matching index, 
#' # then uses that index to return a value from the "return" slot
#' value(E) = 1 # must be set using a value from the allowed list
#' value(E) # returns "one"
#' value(E) = 2
#' value(E) # returns "two"
#' E$selected # returns 2
#'

#' @param allowed A list of allowed values
#' @inheritParams entity
#' @rdname enum
enum = function(name, description=character(0), type=class(value), 
    value=return[[1]],max_length=Inf,allowed=1,selected=1,
    return=allowed,...) {
    
        # new object
    out = .enum(
        name=name, 
        description=description,
        type=type,
        value=value,
        max_length=max_length,
        allowed=allowed,
        selected=selected,
        return=return,
        ...
    )
    return(out)
}

.enum<-setClass(
    "enum",
    slots = c(
        allowed='ANY',
        selected='ANY',
        return = 'ANY'),
    contains = 'entity',
    prototype = list(
        name = 'name not provided',
        description = 'no description provided',
        .params = 'selected',
        .outputs = 'return'
        ),
    validity = function(object) {
        check_selected = length(object@selected != 1)
        check_list = object@selected %in% object@allowed
        check_return = length(object@return) == length(object@allowed)

        # check enum validity
        msg = TRUE
        if (!check_selected) {
            msg = paste0(object$name,': enum a single allowed value must be selected.')
        }
        if (!check_list) {
            msg = paste0(object$name,': enum value must be in enum allowed')
        }
        if (!check_return) {
            msg = paste0(object$name,': enum selected must be between 1 and ',length(object@allowed))
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
        if (value %in% obj@allowed) {
            obj@selected = value
            obj@value = obj@return[[which(obj@allowed == obj@selected)]]
        } else {
            stop(value,' is not in the "allowed" list for this enum.')
        }
        return(obj)
    }
)

setMethod(f = 'show',
    signature = c('enum'),
    definition = function(object) {
        callNextMethod()
        
        cat('allowed:      ',paste0(object@allowed,collapse=', '))
        cat('\n')
        cat('return:       ',paste0(object@return,collapse=', '))
        cat('\n')
        cat('selected:     ',paste0(object@selected,collapse=', '))
        cat('\n')
    }
)

#' @rdname enum
#' @export
setMethod(f = "allowed",
          signature = c("enum"),
          definition = function(obj) {
              return(obj@allowed)
          }
)
