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
    slots=c('value'),
    contains='struct_class',
    prototype=list(name='name not provided',
        description='no description provided')
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
        return(obj)
    }
)


