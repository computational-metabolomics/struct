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

#' @export
setMethod(f="chart.names",
          signature=c("struct_class"),
          definition=function(obj)
          {
            x=showMethods(f=chart.plot,classes=class(obj)[1],printTo=FALSE)
            cat('struct chart objects available for "',class(obj)[1],'" objects:\n',sep='')
            if (x[2]=='<No methods>') {
              cat(' !!No charts are available for this object!!\n')
            } else {

              for (i in 2:length(x)) {
                a=strsplit(x[i],'\"')[[1]]
                if (length(a)>0) {
                  a=a[seq(2, length(a), by=2)]
                  a=a[-which(a==class(obj)[1])]
                  if (is(eval(parse(text=paste0(a,'()'))),'chart')) {
                    cat(' ',a,'\n',sep='')
                  }
                }
              }
            }
          }
)

#' @export
setMethod(f="show",
          signature=c("struct_class"),
          definition=function(object)
          {
            cat('A ',class(object),' object\nName: ',name(object),'\nDescription: ',description(object),sep='')
          }
)
