#' outputs_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental
#' methods for getting/setting outputs etc and should not be called directly.
#' @export outputs_class
#' @param obj a struct object with outputs
#' @param name output id
#' @param x object of class output_class
#' @param value value
#' @include generics.R struct_class.R entity_class.R

outputs_class<-setClass(
    "outputs_class")

#' @describeIn outputs_class get an output as an object (if appropriate)
#' @export
#' @examples
#' M = example_model()
#' obj = output_obj(M,'result_1')
#' @return the output id as an (e.g entity) object
setMethod(f = "output_obj",
    signature = c("outputs_class","character"),
    definition = function(obj,name) {
        value = slot(obj, paste("outputs",name,sep = '_'))
        return(value)
    }
)

#' @describeIn outputs_class set the value of an output
#' @export
#' @examples
#' M = example_model()
#' output_obj(M,'result_1') = entity()
#'
#' @return the modified object
setMethod(f = "output_obj<-",
    signature = c("outputs_class","character"),
    definition = function(obj,name,value) {
        p = slot(obj, paste("outputs",name,sep = '_'))
        slot(obj, paste("outputs",name,sep = '_')) = value
        return(obj)
    }
)

#' @describeIn outputs_class check if a name is a valid output id for an object
#' @export
#' @examples
#' M = model()
#' is_output(M,'example') # FALSE
#' @return logical
setMethod(f = "is_output",
    signature = c("outputs_class"),
    definition = function(obj,name) {
        valid = (output_ids(obj))
        if (name %in% valid) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
)

#' @describeIn outputs_class list the valid output ids for an object
#' @export
#' @examples
#' M = model()
#' output_ids(M)
#' @return character list of valid output ids for object M
setMethod(f = "output_ids",
    signature = c("outputs_class"),
    definition = function(obj) {
        s = slotNames(obj)
        
        # substitute the first _ with a *
        s=sub('_','*',s)
        
        # split by the *
        t = strsplit(s,'\\*')
        
        found = unlist(lapply(t,function(x) {
            'outputs' %in% x
        }))
        t = unlist(t[found])
        t = t[t!= 'outputs']
        return(t)
    }
)

#' @describeIn outputs_class get the (long) name of an output id
#' @export
#' @examples
#' M = example_model()
#' output_name(M,'result_1')
#' @return (long) name of output
setMethod(f = "output_name",
    signature = c("outputs_class",'character'),
    definition = function(obj,name) {
        p = slot(obj, paste("outputs",name,sep = '_'))
        # if the output is an entity then get its name
        if (is(p,'entity')) {
            value = name(p)
        } else {
            # otherwise just return the slot name
            return(name)
        }
        return(value)
    }
)

#' @describeIn outputs_class get the output values of an object as a named list
#' @export
#' @examples
#' M = model()
#' L = output_list(M)
#' @return named list of output ids and current values
setMethod(f = 'output_list',
    signature = c('outputs_class'),
    definition = function(obj) {
        L = list()
        names = output_ids(obj)
        for (i in seq_len(length(names))) {
            L[[names[[i]]]] = output_value(obj,names[[i]])
        }
        return(L)
    }
)

#' @describeIn outputs_class set the output values of an object using a named
#' list
#' @examples
#' M = model()
#' L = output_list(M)
#' @return named list of output ids and current values
setMethod(f = 'output_list<-',
    signature = c('outputs_class','list'),
    definition = function(obj,value) {
        namez = names(value)
        for (i in seq_len(length(namez))) {
            output_value(obj,namez[[i]]) = value[[i]]
        }
        return(obj)
    }
)

#' @describeIn outputs_class get the value of an output by id
#' @export
#' @examples
#' M = example_model()
#' L = output_value(M,'result_1')
#' @return value of output
setMethod(f = "output_value",
    signature = c("outputs_class","character"),
    definition = function(obj,name) {
        
        p = slot(obj, paste("outputs",name,sep = '_'))
        # if the output is an entity then set its value
        if (is(p,'entity')) {
            value = value(p)
        } else {
            # otherwise just set it to the value
            value = slot(obj, paste("outputs",name,sep = '_'))
        }
        return(value)
    }
)

#' @describeIn outputs_class get the value of an output by id
#' @export
#' @examples
#' M = example_model()
#' v = M$result_1
#' @return value of output
setMethod(f = "$",
    signature(x = 'outputs_class'),
    definition = function(x,name) {
        if (is(x,'parameter_class')) {
            if (is_param(x,name)) {
                value = param_value(x,name)
                return(value)
            }
        }
        if (is_output(x,name)) {
            value = output_value(x,name)
            return(value)
        }
        stop(paste0('"',name,'" is not a valid param or output for ', class(x),
            ' objects.'))
    }
)

#' @describeIn outputs_class set the value of an output by id
#' @export
#' @examples
#' M = example_model()
#' output_value(M,'result_1') = dataset()
#' @return modified model object
setMethod(f = "output_value<-",
    signature = c("outputs_class","character"),
    definition = function(obj,name,value) {
        p = slot(obj, paste("outputs",name,sep = '_'))
        # if the parameter is an entity then set its value
        if (is(p,'entity')) {
            value(p) = value
            slot(obj, paste("outputs",name,sep = '_')) = p
        } else {
            # otherwise just set it to the value
            slot(obj, paste("outputs",name,sep = '_')) = value
        }
        return(obj)
    }
)



#' @describeIn outputs_class set the value of an output by id
#' @export
#' @examples
#' M = example_model()
#' M$result_1 = dataset()
#' @return modified model object
setMethod(f = "$<-",
    signature = c(x = "outputs_class"),
    definition = function(x,name,value) {
        if (is(x,'parameter_class')) {
            if (is_param(x,name)) {
                param_value(x,name) = value
                return(x)
            }
        }
        
        if (is_output(x,name)) {
            output_value(x,name) = value
            return(x)
        }
        
        # if we get here then error
        stop(paste0('"',name,'" is not a valid param or output for ', class(x),
            ' objects.'))
        
    }
)

