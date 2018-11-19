#' struct_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental methods for getting/setting parameters etc and should not be called directly.
#' @export struct_class
#' @import methods
#' @include generics.R

struct_class<-setClass(
  "struct_class",
  slots=c(name='character',
          description="character",
          type="character"
          )
)

#' name
#'
#' get the name of an object
#' @rdname object_name
#' @export
#' @param obj a struct object
#' @param value value
setMethod(f="name",
          signature="struct_class",
          definition=function(obj)
          {
            return(obj@name)
          }
)

#' @rdname object_name
#' @export
setMethod(f="name<-",
          signature=c("struct_class"),
          definition=function(obj,value)
          {
            obj@name<-value
            return(obj)
          }
)

#' description
#'
#' get the description of an object
#' @rdname object_desc
#' @export
#' @param obj a struct object
#' @param value value
setMethod(f="description",
          signature="struct_class",
          definition=function(obj)
          {
            return(obj@description)
          }
)

#' @rdname object_desc
#' @export
setMethod(f="description<-",
          signature=c("struct_class"),
          definition=function(obj,value)
          {
            obj@description<-value
            return(obj)
          }
)

#' type
#'
#' get the type of an object
#' @export
#' @param obj a struct object
#' @param value value
setMethod(f="type",
          signature="struct_class",
          definition=function(obj)
          {
            return(obj@type)
          }
)

#' @describeIn type set the type of an object
#' @export
setMethod(f="type<-",
          signature=c("struct_class"),
          definition=function(obj,value)
          {
            obj@type<-value
            return(obj)
          }
)

#' chart names
#'
#' print a list of chart objects associated with the input object
#' @param obj a chart object
#' @param ret a string indicating whether to return a list of chart names (ret="char") or a list of chart objects (ret="obj"). The default is "char".
#' @export
setMethod(f="chart.names",
          signature=c("struct_class"),
          definition=function(obj,ret='char')
          {
            if (ret=='char') {
              OUT=character(0)
            } else if (ret=='obj') {
              OUT=list()
            } else {
              stop('not a valid ret option. Try "char" or "obj"')
            }
            x=showMethods(f=chart.plot,classes=class(obj)[1],printTo=FALSE)
            #cat('struct chart objects available for "',class(obj)[1],'" objects:\n',sep='')
            if (x[2]=='<No methods>') {
              #cat(' !!No charts are available for this object!!\n')
            } else {

              for (i in 2:length(x)) {
                a=strsplit(x[i],'\"')[[1]]
                if (length(a)>0) {
                  a=a[seq(2, length(a), by=2)]
                  a=a[-which(a==class(obj)[1])]
                  if (is(eval(parse(text=paste0(a,'()'))),'chart')) {
                    if (ret=='char') {
                    OUT=c(OUT,a)
                    } else
                    OUT[[length(OUT)+1]]=eval(parse(text=paste0(a,'()')))
                  }
                }
              }
            }
            return(OUT)
          }
)

#' object summary
#'
#' print a brief summary of a struct object
#' @export
setMethod(f="show",
          signature=c("struct_class"),
          definition=function(object)
          {
            cat('A ',class(object),' object\nName: ',name(object),'\nDescription: ',description(object),sep='')
          }
)
