#' @include struct_class.R generics.R

#' @describeIn param_obj 
#' @export
setMethod(f = "param_obj<-",
    signature = c("struct_class","character"),
    definition = function(obj,name,value) {
        p = slot(obj, name)
        if (is_param(obj,name)) {
            slot(obj, name) = value
        }
        return(obj)
    }
)

#' @export
#' @describeIn param_obj 
setMethod(f = "param_obj",
    signature = c("struct_class","character"),
    definition = function(obj,name) {
        value = slot(obj,name)
        return(value)
    }
)


#' @export
#' @describeIn is_param 
setMethod(f = "is_param",
    signature = c("struct_class"),
    definition = function(obj,name) {
        
        # include params set for parent objects
        parents = is(obj)
        w=which(parents == 'struct_class')
        
        valid=NULL
        for (k in 1:w) {
            
            # skip stato
            if (parents[k]=='stato') {
                next
            }
            
            valid = c(valid,param_ids(new_struct(parents[k])))
        }

        # if valid param_id then return true
        return(name %in% valid)
    }
)

#' @export
#' @describeIn param_ids 
setMethod(f = "param_ids",
    signature = c("struct_class"),
    definition = function(obj) {
        t = obj@.params
        return(t)
    }
)


#' @export
#' @describeIn param_name 
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

#' @export
#' @describeIn param_list 
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

#' @export
#' @describeIn param_list 
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

#' @export
#' @describeIn param_value 
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

#' @export
#' @describeIn param_value 
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



