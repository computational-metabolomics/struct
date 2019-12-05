#' parameter_class
#'
#' A base class in the \pkg{struct} package. Should be inherited by other
#' classes and not called directly.
#'
#' The parameter class is a mechanism for providing easily accessible
#' slots in other classes such as methods and models. The idea is that params
#' (and outputs) are slots intended to be accessed by the user without the
#' developer needing to provide generic functions.
#' @export parameter_class
#' @param obj,x parameter_class object
#' @param name id of parameter
#' @param value value
#' @include generics.R struct_class.R
#' @return The return value varies depending on method used for this class
#' @examples
#' # define a new class and inherit the parameter_class
#' example_class = setClass(
#'     'example_class',                     # name of the class
#'     contains = 'parameter_class',          # inherit the parameter class
#'     slots = c(params_example = 'numeric'), # specify a parameter
#'     prototype = list(params_example = 10)   # initial value for parameter
#' )
#'
#' # create an instance of the object
#' M = example_class()
#'
#' # Methods for the parameter class can now be used on the object
#' param_value(M,'example') # 10
#'
parameter_class<-setClass(
    "parameter_class"
)

#' @describeIn parameter_class a parameter as an object (if appropriate)
#' @export
setMethod(f = "param_obj",
    signature = c("parameter_class","character"),
    definition = function(obj,name) {
        value = slot(obj, paste("params",name,sep = '_'))
        return(value)
    }
)

#' @describeIn parameter_class set a parameter as an object
#' @export
setMethod(f = "param_obj<-",
    signature = c("parameter_class","character"),
    definition = function(obj,name,value) {
        slot(obj, paste("params",name,sep = '_')) = value
        return(obj)
    }
)

#' @describeIn parameter_class check if an id is valid for an object
#' @export
setMethod(f = "is_param",
    signature = c("parameter_class"),
    definition = function(obj,name) {
        valid = (param_ids(obj))
        
        # ifvalid param_id then return true
        if (name %in% valid) {
            return(TRUE)
        }
        
        # otherwise return false
        return(FALSE)
        
    }
)

#' @describeIn parameter_class list the valid ids for an object
#' @export
setMethod(f = "param_ids",
    signature = c("parameter_class"),
    definition = function(obj) {
        # get slotnames
        s = slotNames(obj)
        
        # substitute the first _ with a *
        s=sub('_','*',s)
        
        # split by the *
        t = strsplit(s,'\\*')
        
        # see if slot has "params" in name
        found = unlist(lapply(t,function(x) {
            'params' %in% x
        }))
        
        # unlist those that have
        t = unlist(t[found])
        
        # exclude the params part of the slot names
        t = t[t!= 'params']
        return(t)
    }
)

#' @describeIn parameter_class get the (long) name of a parameter by id
#' @export
setMethod(f = "param_name",
    signature = c("parameter_class",'character'),
    definition = function(obj,name) {
        p = slot(obj, paste("params",name,sep = '_'))
        # if the parameter is an entity then get its name
        if (is(p,'entity')) {
            value = name(p)
            return(value)
        }
        
        # otherwise just return the slot name
        return(name)
    }
)

#' @describeIn parameter_class get a named list of parameter values for an
#' object
#' @export
setMethod(f = 'param_list',
    signature = c('parameter_class'),
    definition = function(obj) {
        L = list()
        names = param_ids(obj)
        for (i in seq_len(length(names))) {
            L[[names[[i]]]] = param_value(obj,names[[i]])
        }
        return(L)
    }
)

#' @describeIn parameter_class set parameter values for an object using a named
#' list
#' @export
setMethod(f = 'param_list<-',
    signature = c('parameter_class','list'),
    definition = function(obj,value) {
        namez = names(value)
        for (i in seq_len(length(namez))) {
            param_value(obj,namez[[i]]) = value[[i]]
        }
        return(obj)
    }
)

#' @describeIn parameter_class get the value for a parameter by id
#' @export
setMethod(f = "param_value",
    signature = c("parameter_class","character"),
    definition = function(obj,name) {
        p = slot(obj, paste("params",name,sep = '_'))
        
        # if the parameter is an entity then set its entity value
        if (is(p,'entity')) {
            value = value(p)
        }
        else {
            # otherwise just set it to the value
            value = slot(obj, paste("params",name,sep = '_'))
        }
        return(value)
    }
)

#' @describeIn parameter_class get the value for a parameter by id
#' @export
setMethod(f = "$",
    signature(x = 'parameter_class'),
    definition = function(x,name) {
        if (is(x,'outputs_class')) {
            if (is_output(x,name)) {
                value = output_value(x,name)
                return(value)
            }
        }
        if (is_param(x,name)) {
            value = param_value(x,name)
            return(value)
        }
        stop(paste0('"',name,'" is not a valid param or output for ', class(x),
            ' objects.'))
    }
)

#' @describeIn parameter_class set the value for a parameter by id
#' @export
setMethod(f = "param_value<-",
    signature = c("parameter_class","character","ANY"),
    definition = function(obj,name,value) {
        p = slot(obj, paste("params",name,sep = '_'))
        # if the parameter is an entity then set its value
        if (is(p,'entity')) {
            value(p) = value
            slot(obj, paste("params",name,sep = '_')) = p
        }
        else {
            # otherwise just set it to the value
            slot(obj, paste("params",name,sep = '_')) = value
        }
        return(obj)
    }
)



#' @describeIn parameter_class set the value for a parameter by id
#' @export
setMethod(f = "$<-",
    signature = c(x = "parameter_class"),
    definition = function(x,name,value) {
        if (is(x,'outputs_class')) {
            if (is_output(x,name)) {
                output_value(x,name) = value
                return(x)
            }
        }
        
        if (is_param(x,name)) {
            param_value(x,name) = value
            return(x)
        }
        
        # if we get here then error
        stop(paste0('"',name,'" is not a valid param or output for ', class(x),
            ' objects.'))
    }
)
