#' parameter_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental methods for getting/setting parameters etc and should not be called directly.
#' @export parameter_class
#' @include generics.R struct_class.R
#'

parameter_class<-setClass(
  "parameter_class"
)

## initialise parameters on object creation
setMethod(f="initialize",
          signature="parameter_class",
          definition=function(.Object,...)
          {
            L=list(...)
            if (length(L)>0)
            {
              for (i in 1:length(L))
              {
                param.value(.Object,names(L)[[i]])=L[[names(L)[[i]]]]
              }
            }
            return(.Object)
          }
)

#' @export
setMethod(f="param.obj",
          signature=c("parameter_class","character"),
          definition=function(obj,name)
          {
            value=slot(obj, paste("params",name,sep='.'))
            return(value)
          }
)

#' @export
setMethod(f="param.obj<-",
  signature=c("parameter_class","character"),
  definition=function(obj,name,value)
  {
    slot(obj, paste("params",name,sep='.'))=value
    return(obj)
  }
)

#' @export
setMethod(f="is.param",
          signature=c("parameter_class"),
          definition=function(obj,name)
          {
            valid=(param.ids(obj))
            if (name %in% valid)
            {return(TRUE)}
            else
            {return(FALSE)}
          }
)

#' @export
setMethod(f="param.ids",
          signature=c("parameter_class"),
          definition=function(obj)
          {
            s=slotNames(obj)
            t=strsplit(s,'\\.')
            found=unlist(lapply(t,function(x) {'params' %in% x}))
            t=unlist(t[found])
            t=t[t!='params']
            return(t)
          }
)

#' @export
setMethod(f="param.name",
          signature=c("parameter_class",'character'),
          definition=function(obj,name)
          {
            p=slot(obj, paste("params",name,sep='.'))
            # if the parameter is an entity then get its name
            if (is(p,'entity'))
            {
              value=name(p)
            }
            else
            {
              # otherwise just return the slot name
              value=slot(obj, paste("params",name,sep='.'))
            }
            return(value)
          }
)

#'@export
setMethod(f='param.list',
          signature=c('parameter_class'),
          definition=function(obj)
          {
            L=list()
            names=param.ids(obj)
            for (i in 1:length(names))
            {
              L[[names[[i]]]]=param.value(obj,names[[i]])
            }
            return(L)
          }
)

#'@export
setMethod(f='param.list<-',
          signature=c('parameter_class','list'),
          definition=function(obj,value)
          {
            namez=names(value)
            for (i in 1:length(namez))
            {
              param.value(obj,namez[[i]])=value[[i]]
            }
            return(obj)
          }
)

#' @export
setMethod(f="param.value",
          signature=c("parameter_class","character"),
          definition=function(obj,name)
          {

            p=slot(obj, paste("params",name,sep='.'))
            # if the parameter is an entity then set its value
            if (is(p,'entity'))
            {
              value=value(p)
            }
            else
            {
              # otherwise just set it to the value
              value=slot(obj, paste("params",name,sep='.'))
            }
            return(value)
          }
)

#' @export
setMethod(f="$",
          signature(x='parameter_class'),
          definition=function(x,name)
          {
            if (is.param(x,name)) {
              value=param.value(x,name)
            } else if (is.output(x,name)) {
              value=output.value(x,name)
            } else {
              stop(paste0('"',name,'" is ot a valid param or output for ', class(x), ' objects.'))
            }
            return(value)
          }
)

#' @export
setMethod(f="param.value<-",
          signature=c("parameter_class","character","missing"),
          definition=function(obj,name,value)
          {
            p=slot(obj, paste("params",name,sep='.'))
            # if the parameter is an entity then set its value
            if (is(p,'entity'))
            {
              value(p)=value
              slot(obj, paste("params",name,sep='.'))=p
            }
            else
            {
              # otherwise just set it to the value
              slot(obj, paste("params",name,sep='.'))=value
            }
            return(obj)
          }
)

#' @export
setMethod(f="$<-",
          signature=c(x="parameter_class"),
          definition=function(x,name,value)
          {
            if (is.param(x,name)) {
              param.value(x,name)=value
            } else if (is.output(x,name)) {
              output.value(x,name)=value
            } else {
              stop(paste0('"',name,'" is ot a valid param or output for ', class(x), ' objects.'))
            }
            return(x)
          }
)

#' @export
setMethod(f="param.value<-",
          signature=c("parameter_class","character",'numeric'),
          definition=function(obj,name,idx,value)
          {
            M=models(obj)
            #if its an iterator, pass the request trhogh to the models
            if(is(obj,'iterator') | is(obj,'model.seq'))
            {
              param.value(M,name,idx)=value
            }
            else # must be model
            {
              param.value(M,name)=value
            }
            models(obj)=M
            return(obj)
          }
)

