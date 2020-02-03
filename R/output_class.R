#' @include generics.R struct_class.R
#' 
#' @describeIn output_obj 
#' @export
setMethod(f = "output_obj",
    signature = c("struct_class","character"),
    definition = function(obj,name) {
        value = slot(obj,name)
        return(value)
    }
)

#' @describeIn output_obj 
#' @export
#' @return the modified object
setMethod(f = "output_obj<-",
    signature = c("struct_class","character"),
    definition = function(obj,name,value) {
        p = slot(obj, name)
        if (is_output(obj,name)) {
            slot(obj, name) = value
        }
        return(obj)
    }
)

#' @describeIn is_output 
#' @export
setMethod(f = "is_output",
    signature = c("struct_class"),
    definition = function(obj,name) {
        valid = output_ids(obj)
        if (length(valid)>0) {
            return(name %in% valid)            
        } else {
            return(FALSE)
        }
    }
)

#' @describeIn output_ids 
#' @export
setMethod(f = "output_ids",
    signature = c("struct_class"),
    definition = function(obj) {
        # get list of outputs
        t = obj@.outputs
        return(t)
    }
)

#' @describeIn output_name 
#' @export
setMethod(f = "output_name",
    signature = c("struct_class",'character'),
    definition = function(obj,name) {
        p = slot(obj, name)
        # if the output is an entity then get its name
        if (is(p,'entity')) {
            value = p$name
        } else {
            # otherwise just return the slot name
            return(name)
        }
        return(value)
    }
)

#' @describeIn output_list 
#' @export
setMethod(f = 'output_list',
    signature = c('struct_class'),
    definition = function(obj) {
        L = list()
        names = output_ids(obj)
        for (i in seq_len(length(names))) {
            L[[names[[i]]]] = output_value(obj,names[[i]])
        }
        return(L)
    }
)

#' @describeIn output_list 
#' @export
setMethod(f = 'output_list<-',
    signature = c('struct_class','list'),
    definition = function(obj,value) {
        namez = names(value)
        for (i in seq_len(length(namez))) {
            output_value(obj,namez[[i]]) = value[[i]]
        }
        return(obj)
    }
)

#' @describeIn output_value 
#' @export
setMethod(f = "output_value",
    signature = c("struct_class","character"),
    definition = function(obj,name) {
        
        p = slot(obj, name)
        # if the output is an entity then set its value
        if (is(p,'entity')) {
            value = value(p)
        } else {
            # otherwise just set it to the value
            value = slot(obj, name)
        }
        return(value)
    }
)


#' @describeIn output_value 
#' @export
setMethod(f = "output_value<-",
    signature = c("struct_class","character"),
    definition = function(obj,name,value) {
        p = slot(obj, name)
        # if the parameter is an entity then set its value
        if (is(p,'entity')) {
            value(p) = value
            slot(obj, name) = p
        } else {
            # otherwise just set it to the value
            slot(obj, name) = value
        }
        return(obj)
    }
)



