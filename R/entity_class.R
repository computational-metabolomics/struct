#' entity_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export entity
#' @param MET an entity object
#' @param value value of the entity
#' @param obj entity object
#' @include generics.R struct_class.R
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
          definition=function(MET)
          {
            return(MET@value)
          }
)

#' @describeIn entity set the  value for an entity
#' @export
setMethod(f="value<-",
          signature=c("entity"),
          definition=function(obj,value)
          {
            obj@value=value
            return(obj)
          }
)


