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
#' @import rlang
#' @inheritParams struct_class
#' @param obj An entity object
#' @param max_length Maximum length of value vector (default 1)
#' @param value The value of the parameter/outputs
#' @param ... additional inputs to the struct_class object
#' @include generics.R struct_class.R parameter_class.R
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
        constraints=list(
            type_equals,
            maximum_length
        ),
        ...) {

    value=check_init_val(value,type)

    # new object
    out = .entity(
        name=name,
        description=description,
        type=type,
        value=value,
        max_length=max_length,
        constraints=constraints,
        ...
    )
    return(out)
}

.entity<-setClass(
    "entity",
    slots = c(value = 'ANY',max_length = 'numeric',constraints='list'),
    contains = 'struct_class',
    prototype = list(
        name = 'name not provided',
        description = 'no description provided',
        value = character(0),
        type = 'character',
        max_length = Inf,
        ontology=character()),
    validity = function(object) {

        msg = character(0)
        for (k in object@constraints){
            check = eval(k$ex,env=list2env(slots(object)))
            if (!check) {
                msg=c(
                    msg,
                    paste0('"',object$name,
                           '" is not valid because ',
                           eval(k$msg,env=list2env(slots(object)))
                    )
                )
            }
        }
        if (length(msg)==0) {
            return(TRUE)
        } else {
            return(msg)
        }
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


#' Entity constraints
#'
#' This helper function creates a list of quosures that can be used to apply
#' constraints to an entity object.
#'
#' @export
#' @param ... any number of constraints e.g. value<6
#' @returns A list of quosures that will be evaluated to check validity of the entity
#' @examples
#' # ensure the value of an entity is exactly 6
#' constraint(value==6)
#'
#' # check the length
#' constraint(length(value)==1)
#'
#'
entity_constraint=function(ex,msg){
    return(
        list(
            ex = enexpr(ex),
            msg = enexpr(msg)
        )
    )
}



# function to return all slots as a list
slots=function(object){
    n = slotNames(object)
    S = lapply(n,function(x){
        return(slot(object,x))
    })
    names(S)=n
    return(S)
}

# function to expand inherits to vectors/lists
elements_inherit = function(x, class_name) {
    sapply(x, function(el) inherits(el, class_name,which=TRUE))
}

# check that all elements of a vector inherit one of the classes in 'type' INCLUDES LISTS
vector_type_equals = entity_constraint(
    ex = all(elements_inherit(value,type)) | ('ANY' %in% type),
    msg = paste0('all elements in "value" must be ', paste0('"',type,'"',collapse=' or '),'.'))

# check that the value is of the correct type
type_equals = entity_constraint(
    ex = inherits(value,type,which = TRUE) | ('ANY' %in% type),
    msg = paste0('"value" must be ', paste0('"',type,'"',collapse=' or '),'.')
)

# check length does not exceed value
maximum_length = entity_constraint(
    ex = length(value) <= max_length,
    msg = paste0('the length of "value" must be less than or equal to ', max_length)
)

# check minimum value
minimum_value = function(x) {
    entity_constraint(
        ex = all(value >= !!x),
        msg = paste0('"value" must be greater than ', !!x)
    )
}
