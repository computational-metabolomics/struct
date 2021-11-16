#' Entity objects
#'
#' A base class in the \pkg{struct} package. Not normally called directly.
#' An entity object is used to store information about a parameter or output_
#' The standard 'name','description' and 'type' slots are included, along with
#' 'value' for storing the value of the parameter and 'max_length' for restricting
#' the length of 'value' if needed.
#'
#' Entity objects are usually defined in the prototype of another object, but
#' can be extracted using \code{param_obj} and \code{output_obj}.
#'
#' @export
#' @inheritParams struct_class
#' @param obj An entity object
#' @param max_length Maximum length of value vector (default 1)
#' @param value The value of the parameter/outputs
#' @param ... additional inputs to the struct_class object
#' @include generics.R struct_class.R
#' @return An entity object
#' @examples
#' # Create a new entity object
#' E = entity(
#'     name = 'example',
#'     description = 'this is an example',
#'     type = 'numeric',
#'     value = 1
#' )
#'
#' # Get/set the value of the entity object
#' value(E)
#' value(E) = 10
#' @rdname entity
entity = function(
    name, 
    description=character(0), 
    type='character',
    value=NULL,
    max_length=Inf,
    ...) {
    
    value=check_init_val(value,type)
    
    # new object
    out = .entity(
        name=name, 
        description=description,
        type=type,
        value=value,
        max_length=max_length,
        ...
    )
    return(out)
}

.entity<-setClass(
    "entity",
    slots = c(value = 'ANY',max_length = 'numeric'),
    contains = 'struct_class',
    prototype = list(
        name = 'name not provided',
        description = 'no description provided',
        value = character(0),
        type = 'character',
        max_length = Inf,
        ontology=character(),
        .params='value'
    ),
    validity = function(object) {
        check_length = length(value(object)) <= max_length(object)

        check_type = any(unlist(lapply(object$type,function(x) is(value(object),x))))
        
        check_max_length = length(max_length(object)) == 1
        msg = TRUE
        if (!check_length) {
            msg = paste0(object$name,': number of values must be less than "max_length"')
        }
        if (!check_type) {
            msg = paste0(object$type,': class of value must match "type" for this entity.')
        }
        if (!check_max_length) {
            msg = paste0(object$max_length,': ', ' max_length must be of length 1')
        }
        return(msg)
    }
)


check_init_val=function(value,type) {
    if (is.null(value) & !("NULL" %in% type)) {
        if (isVirtualClass(type)) {
            # create a spoof object until a real one is generated
            x=numeric(0)
            class(x)=type
            value=x
        } else {
            value=new(type[[1]])
        }
    }
    return(value)
}


#' @rdname entity
#' @export
setMethod(f = "value",
    signature = c("entity"),
    definition = function(obj) {
        return(obj@value)
    }
)

#' @rdname entity
#' @export
setMethod(f = "value<-",
    signature = c("entity"),
    definition = function(obj,value) {
        obj@value = value
        validObject(obj)
        return(obj)
    }
)

#' @rdname entity
#' @export
setMethod(f = "max_length",
    signature = c("entity"),
    definition = function(obj) {
        return(obj@max_length)
    }
)

#' @rdname entity
#' @export
setMethod(f = "max_length<-",
    signature = c("entity"),
    definition = function(obj,value) {
        obj@max_length = value
        validObject(obj)
        return(obj)
    }
)


setMethod(f = 'show',
    signature = c('entity'),
    definition = function(object) {
        callNextMethod() # force the default output
        
        V=value(object)
        if (is(V,'DatasetExperiment') | is(V,'SummarizedExperiment') | is(V,'matrix')) {
            V=paste0(nrow(V), ' rows x ', ncol(V), ' columns (',class(V),')')
        } else if (is.atomic(V)) {
            V=V
        } else {
            V=class(V)
        }
        
        # add extra info
        cat('value:         ', V, '\n',sep='')
        cat('type:          ', paste0(object$type,collapse=', '), '\n',sep='')
        cat('max length:    ', max_length(object),sep='')
        cat('\n')
    }
)