#' parameter_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental methods for getting/setting parameters etc and should not be called directly.
#' @export parameter_class
#' @param obj parameter_class object
#' @param name id of parameter
#' @param x parameter_class object
#' @param value value
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

#' @describeIn parameter_class a parameter as an object (if appropriate)
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' obj = param.obj(M,'example')
#' }
#' @return returns the parameter as an object (e.g. entity, enum)
#'
setMethod(f="param.obj",
    signature=c("parameter_class","character"),
    definition=function(obj,name)
    {
        value=slot(obj, paste("params",name,sep='.'))
        return(value)
    }
)

#' @describeIn parameter_class set a parameter as an object
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' param.obj(M,'example') = entity()
#' }
#' @return modified model object
setMethod(f="param.obj<-",
    signature=c("parameter_class","character"),
    definition=function(obj,name,value)
    {
        slot(obj, paste("params",name,sep='.'))=value
        return(obj)
    }
)

#' @describeIn parameter_class check if an id is valid for an object
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' is.param(M,'example')
#' }
#' @return logical
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

#' @describeIn parameter_class list the valid ids for an object
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' param.ids(M)
#' }
#' @return character list of parameter ids
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

#' @describeIn parameter_class get the (long) name of a parameter by id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' param.name(M,'example')
#' }
#' @return (long) name of parameter
setMethod(f="param.name",
    signature=c("parameter_class",'character'),
    definition=function(obj,name)
    {

        p=slot(obj, paste("params",name,sep='.'))
        # if the parameter is an entity then get its name
        if (is(p,'entity'))
        {
            value=name(p)
            return(value)
        }
        else
        {
            # otherwise just return the slot name
            return(name)

        }

    }
)

#' @describeIn parameter_class get a named list of parameter values for an object
#'@export
#' @examples
#' \dontrun{
#' M = model()
#' L = param.list(M,'example')
#' }
#' @return named list of parameter ids and values
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

#' @describeIn parameter_class set parameter values for an object using a named list
#'@export
#' @examples
#' \dontrun{
#' M = model()
#' L = list('example' = model())
#' param.list(M,'example') = L
#' }
#' @return modified model object
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

#' @describeIn parameter_class get the value for a parameter by id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' param.value(M,'example')
#' }
#' @return value of parameter
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

#' @describeIn parameter_class get the value for a parameter by id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' M$example
#' }
#' @return value of parameter
setMethod(f="$",
    signature(x='parameter_class'),
    definition=function(x,name)
    {
        if (is(x,'outputs_class')) {
            if (is.output(x,name)) {
                value=output.value(x,name)
                return(value)
            }
        }
        if (is.param(x,name)) {
            value=param.value(x,name)
            return(value)
        }
        stop(paste0('"',name,'" is ot a valid param or output for ', class(x), ' objects.'))
    }
)

#' @describeIn parameter_class set the value for a parameter by id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' param.value(M,'example') = 10
#' }
#' @return modified model object
setMethod(f="param.value<-",
    signature=c("parameter_class","character","ANY"),
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



#' @describeIn parameter_class set the value for a parameter by id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' M$example = 10
#' }
#' @return (long) name of parameter
setMethod(f="$<-",
    signature=c(x="parameter_class"),
    definition=function(x,name,value) {
        if (is(x,'outputs_class')) {
            if (is.output(x,name)) {
                output.value(x,name)=value
                return(x)
            }
        }

        if (is.param(x,name)) {
            param.value(x,name)=value
            return(x)
        }

        # if we get here then error
        stop(paste0('"',name,'" is not a valid param or output for ', class(x), ' objects.'))
    }
)
