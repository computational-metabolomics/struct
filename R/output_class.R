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

        # include params set for parent objects
        valid = output_ids(obj)

        # if valid param_id then return true
        return(name %in% valid)
    }
)

#' @describeIn output_ids
#' @export
setMethod(f = "output_ids",
    signature = c("struct_class"),
    definition = function(obj) {
        # include params set for parent objects
        parents = is(obj)
        w=which(parents == 'struct_class')

        valid=NULL
        for (k in 1:w) {

            # skip stato
            if (parents[k]=='stato') {
                next
            }

            valid = c(valid,new_struct(parents[k])@.outputs)
        }

        return(unique(valid))
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



