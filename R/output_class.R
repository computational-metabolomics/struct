#' @include generics.R struct_class.R
#' @describeIn output_obj get an output as an object (if appropriate)
#' @export
#' @examples
#' M = example_model()
#' obj = output_obj(M,'result_1')
#' @return the output id as an (e.g entity) object
setMethod(f = "output_obj",
    signature = c("struct_class","character"),
    definition = function(obj,name) {
        value = slot(obj,name)
        return(value)
    }
)

#' @describeIn output_obj set the value of an output
#' @export
#' @examples
#' M = example_model()
#' output_obj(M,'result_1') = entity()
#'
#' @return the modified object
setMethod(f = "output_obj<-",
    signature = c("struct_class","character"),
    definition = function(obj,name,value) {
        p = slot(obj, name)
        slot(obj, name) = value
        return(obj)
    }
)

#' @describeIn is_output check if a name is a valid output id for an object
#' @export
#' @examples
#' M = model()
#' is_output(M,'example') # FALSE
#' @return logical
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

#' @describeIn output_ids list the valid output ids for an object
#' @export
#' @examples
#' M = model()
#' output_ids(M)
#' @return character list of valid output ids for object M
setMethod(f = "output_ids",
    signature = c("struct_class"),
    definition = function(obj) {
        # get list of outputs
        t = obj@.outputs
        return(t)
    }
)

#' @describeIn output_name get the (long) name of an output id
#' @export
#' @examples
#' M = example_model()
#' output_name(M,'result_1')
#' @return (long) name of output
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

#' @describeIn output_list get the output values of an object as a named list
#' @export
#' @examples
#' M = model()
#' L = output_list(M)
#' @return named list of output ids and current values
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

#' @describeIn output_list set the output values of an object using a named
#' list
#' @examples
#' M = model()
#' L = output_list(M)
#' @return named list of output ids and current values
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

#' @describeIn output_value get the value of an output by id
#' @export
#' @examples
#' M = example_model()
#' L = output_value(M,'result_1')
#' @return value of output
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


#' @describeIn output_value set the value of an output by id
#' @export
#' @examples
#' M = example_model()
#' output_value(M,'result_1') = DatasetExperiment()
#' @return modified model object
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



