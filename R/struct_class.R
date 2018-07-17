#' struct_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental methods for getting/setting parameters etc and should not be called directly.
#' @export struct_class
#' @include generics.R

struct_class<-setClass(
  "struct_class",
  slots=c(name='character',
          description="character",
          type="character"
          )
)

#' @export
setMethod(f="name",
          signature="struct_class",
          definition=function(obj)
          {
            return(obj@name)
          }
)

#' @export
setMethod(f="name<-",
          signature=c("struct_class"),
          definition=function(obj,value)
          {
            obj@name<-value
            return(obj)
          }
)

#' @export
setMethod(f="description",
          signature="struct_class",
          definition=function(obj)
          {
            return(obj@description)
          }
)

#' @export
setMethod(f="description<-",
          signature=c("struct_class"),
          definition=function(obj,value)
          {
            obj@description<-value
            return(obj)
          }
)

#' @export
setMethod(f="type",
          signature="struct_class",
          definition=function(obj)
          {
            return(obj@type)
          }
)

#' @export
setMethod(f="type<-",
          signature=c("struct_class"),
          definition=function(obj,value)
          {
            obj@type<-value
            return(obj)
          }
)
