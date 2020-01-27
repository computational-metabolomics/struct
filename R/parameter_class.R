#' @include generics.R struct_class.R
#' @describeIn param_obj get a param as an object (if appropriate)
#' @export
#' @examples
#' M = example_model()
#' obj = output_obj(M,'result_1')
#' @return the output id as an (e.g entity) object
setMethod(f = "param_obj",
    signature = c("struct_class","character"),
    definition = function(obj,name) {
        value = slot(obj,name)
        return(value)
    }
)


#' @describeIn is_param test if named slot is a parameter for an object
#' @export
setMethod(f = "is_param",
    signature = c("struct_class"),
    definition = function(obj,name) {
        
        valid = param_ids(obj)
        
        # if valid param_id then return true
        return(name %in% valid)
    }
)

#' @describeIn param_ids list the valid ids for an object
#' @export
setMethod(f = "param_ids",
    signature = c("struct_class"),
    definition = function(obj) {
        t = obj@.params
        return(t)
    }
)


#' @describeIn param_name get the (long) name of a parameter by id
#' @export
setMethod(f = "param_name",
    signature = c("struct_class",'character'),
    definition = function(obj,name) {
        p = slot(obj, name)
        # if the parameter is an entity then get its name
        if (is(p,'entity')) {
            value = p$name
            return(value)
        }
        
        # otherwise just return the slot name
        return(name)
    }
)

#' @describeIn param_list get a named list of parameter values for an
#' object
#' @export
setMethod(f = 'param_list',
    signature = c('struct_class'),
    definition = function(obj) {
        L = list()
        names = param_ids(obj)
        for (i in seq_len(length(names))) {
            L[[names[[i]]]] = param_value(obj,names[[i]])
        }
        return(L)
    }
)

#' @describeIn param_list set parameter values for an object using a named
#' list
#' @export
setMethod(f = 'param_list<-',
    signature = c('struct_class','list'),
    definition = function(obj,value) {
        namez = names(value)
        for (i in seq_len(length(namez))) {
            param_value(obj,namez[[i]]) = value[[i]]
        }
        return(obj)
    }
)

#' @describeIn param_value get the value for a parameter by id
#' @export
setMethod(f = "param_value",
    signature = c("struct_class","character"),
    definition = function(obj,name) {
        p = slot(obj, name)
        
        # if the parameter is an entity then set its entity value
        if (is(p,'entity')) {
            value = value(p)
        }
        else {
            # otherwise just set it to the value
            value = slot(obj, name)
        }
        return(value)
    }
)



#' @describeIn param_value set the value for a parameter by id
#' @export
setMethod(f = "param_value<-",
    signature = c("struct_class","character","ANY"),
    definition = function(obj,name,value) {
        p = slot(obj,name)
        # if the parameter is an entity then set its value
        if (is(p,'entity')) {
            value(p) = value
            slot(obj, name) = p
        }
        else {
            # otherwise just set it to the value
            slot(obj, name) = value
        }
        return(obj)
    }
)



