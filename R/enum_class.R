#' enum_class
#'
#' A base class in the \pkg{struct} package. should not be called directly.
#' @export enum
#' @include generics.R struct_class.R


enum<-setClass(
  "enum",
  slots=c('list'),
  contains='entity',
  prototype=list(name='name not provided',
                 description='no description provided')
)

## initialise parameters on object creation
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


