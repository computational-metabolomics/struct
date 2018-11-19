#' outputs_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental methods for getting/setting outputs etc and should not be called directly.
#' @export outputs_class
#' @param obj a struct object with outputs
#' @param name output id
#' @param x object of class output_class
#' @param value value
#' @include generics.R struct_class.R entity_class.R

outputs_class<-setClass(
    "outputs_class")

## initialise outputs on object creation
setMethod(f="initialize",
    signature="outputs_class",
    definition=function(.Object,...)
    {
        L=list(...)
        if (length(L)>0)
        {
            for (i in 1:length(L))
            {
                output.value(.Object,names(L)[[i]])=L[[names(L)[[i]]]]
            }
        }
        return(.Object)
    }
)

#' @describeIn outputs_class get an output as an object (if appropriate)
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' obj = output.obj(M,'example')
#' }
#' @return the output id as an (e.g entity) object
setMethod(f="output.obj",
    signature=c("outputs_class","character"),
    definition=function(obj,name)
    {
        value=slot(obj, paste("outputs",name,sep='.'))
        return(value)
    }
)

#' @describeIn outputs_class set the value of an output
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' output.obj(M,'example') = entity()
#' }
#' @return the modified object
setMethod(f="output.obj<-",
    signature=c("outputs_class","character"),
    definition=function(obj,name,value)
    {
        p=slot(obj, paste("outputs",name,sep='.'))
        slot(obj, paste("outputs",name,sep='.'))=value
        return(obj)
    }
)

#' @describeIn outputs_class check if a name is a valid output id for an object
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' is.output(M,'example') # FALSE
#' }
#' @return logical
setMethod(f="is.output",
    signature=c("outputs_class"),
    definition=function(obj,name)
    {
        valid=(output.ids(obj))
        if (name %in% valid)
        {return(TRUE)}
        else
        {return(FALSE)}
    }
)

#' @describeIn outputs_class list the valid output ids for an object
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' output.ids(M)
#' }
#' @return character list of valid output ids for object M
setMethod(f="output.ids",
    signature=c("outputs_class"),
    definition=function(obj)
    {
        s=slotNames(obj)
        t=strsplit(s,'\\.')
        found=unlist(lapply(t,function(x) {'outputs' %in% x}))
        t=unlist(t[found])
        t=t[t!='outputs']
        return(t)
    }
)

#' @describeIn outputs_class get the (long) name of an output id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' output.name(M,'example')
#' }
#' @return (long) name of output
setMethod(f="output.name",
    signature=c("outputs_class",'character'),
    definition=function(obj,name)
    {
        p=slot(obj, paste("outputs",name,sep='.'))
        # if the output is an entity then get its name
        if (is(p,'entity'))
        {
            value=name(p)
        }
        else
        {
            # otherwise just return the slot name
            value=slot(obj, paste("outputs",name,sep='.'))
        }
        return(value)
    }
)

#' @describeIn outputs_class get the output values of an object as a named list
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' L = output.list(M)
#' }
#' @return named list of output ids and current values
setMethod(f='output.list',
    signature=c('outputs_class'),
    definition=function(obj)
    {
        L=list()
        names=output.ids(obj)
        for (i in 1:length(names))
        {
            L[[names[[i]]]]=output.value(obj,names[[i]])
        }
        return(L)
    }
)

#' @describeIn outputs_class set the output values of an object using a named list
#' @examples
#' \dontrun{
#' M = model()
#' L = output.list(M)
#' }
#' @return named list of output ids and current values
setMethod(f='output.list<-',
    signature=c('outputs_class','list'),
    definition=function(obj,value)
    {
        names=name(value)
        for (i in 1:length(names))
        {
            output.value(obj,names[[i]])=value[[i]]
        }
        return(obj)
    }
)

#' @describeIn outputs_class get the value of an output by id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' L = output.value(M,'example')
#' }
#' @return value of output
setMethod(f="output.value",
    signature=c("outputs_class","character"),
    definition=function(obj,name)
    {

        p=slot(obj, paste("outputs",name,sep='.'))
        # if the output is an entity then set its value
        if (is(p,'entity'))
        {
            value=value(p)

        }
        else
        {
            # otherwise just set it to the value
            value=slot(obj, paste("outputs",name,sep='.'))
        }
        return(value)
    }
)

#' @describeIn outputs_class get the value of an output by id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' v = M$example
#' }
#' @return value of output
setMethod(f="$",
    signature(x='outputs_class'),

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

#' @describeIn outputs_class set the value of an output by id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' output.value(M,'example') = 10
#' }
#' @return modified model object
setMethod(f="output.value<-",
    signature=c("outputs_class","character"),
    definition=function(obj,name,value)
    {
        p=slot(obj, paste("outputs",name,sep='.'))
        # if the parameter is an entity then set its value
        if (is(p,'entity'))
        {
            value(p)=value
            slot(obj, paste("outputs",name,sep='.'))=p
        }
        else
        {
            # otherwise just set it to the value
            slot(obj, paste("outputs",name,sep='.'))=value
        }
        return(obj)
    }
)

#' @describeIn outputs_class set the value of an output by id
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' M$example = 10
#' }
#' @return modified model object
setMethod(f="$<-",
    signature=c(x="outputs_class"),
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

