#' Entity objects
#'
#' A base class in the \pkg{struct} package. Not normally called directly.
#'
#' An entity object is used to store information about a parameter or output.
#' The standard 'name','description' and 'type' slots are included, along with
#' 'value' for storing the value of the parameter.
#'
#' Entity objects are usually defined in the prototype of another object, but
#' can be extracted using \code{param.obj} and \code{output.obj}.
#'
#' @export entity
#' @param obj An entity object
#' @param value Value of the entity
#' @param max_length maximum length of value vector (default 1)
#' @include generics.R struct_class.R
#' @return and entity object
#' @examples
#' # Create a new entity object
#' E = entity(
#'     name='example',
#'     description='this is an example',
#'     type='numeric',
#'     value=1
#' )
#'
#' # Get/set the value of the entity object
#' value(E)
#' value(E) = 10
#'
entity<-setClass(
    "entity",
    slots=c(value='ANY',max_length='numeric'),
    contains='struct_class',
    prototype=list(name='name not provided',
        description='no description provided',
        value='',
        type='character',
        max_length=1
    ),
    validity = function(object) {
        check_length=length(value(object)) <= max_length(object)
        check_type=class(value(object))[1] %in% type(object)
        check_max_length=length(max_length(object))==1
        msg=TRUE
        if (!check_length) {
            msg=paste0(name(object),': number of values must be less than "max_length"')
        }
        if (!check_type) {
            msg=paste0(name(object),': class of value must match "type"')
        }
        if (!check_max_length) {
            msg=paste0(name(object),': ', ' max_length must be of length 1')
        }
        return(msg)
    }
)

## initialise parameters on object creation
setMethod(f="initialize",
    signature="entity",
    definition=function(.Object,...)
    {
        L=list(...)
        SN=slotNames(.Object)
        if (length(L)>0)
        {
            for (i in seq_len(length(L)))
            {
                if (names(L)[[i]] %in% SN) {
                    slot(.Object,names(L)[[i]])=L[[names(L)[[i]]]]
                }
            }
        }

        if (!('value' %in% names(L))) {
            if (isVirtualClass(.Object@type)) {
                # create a spoof object until a real one is generated
                x=numeric(0)
                class(x)=.Object@type
                .Object@value=x
            } else {
                .Object@value=new(.Object@type)
            }
        }
        validObject(.Object)
        return(.Object)
    }
)

#' @describeIn entity get the value for an entity
#' @export
setMethod(f="value",
    signature=c("entity"),
    definition=function(obj)
    {
        return(obj@value)
    }
)

#' @describeIn entity set the value for an entity
#' @export
setMethod(f="value<-",
    signature=c("entity"),
    definition=function(obj,value)
    {
        obj@value=value
        validObject(obj)
        return(obj)
    }
)

#' @describeIn entity get the maximum length of value vector for an entity
#' @export
setMethod(f="max_length",
    signature=c("entity"),
    definition=function(obj)
    {
        return(obj@max_length)
    }
)

#' @describeIn entity set the maximum length of value vector for an entity
#' @export
setMethod(f="max_length<-",
    signature=c("entity"),
    definition=function(obj,value)
    {
        obj@max_length=value
        validObject(obj)
        return(obj)
    }
)
