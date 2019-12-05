#' struct_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental
#' methods for getting/setting parameters etc and should not be called directly.
#' @export struct_class
#' @import methods
#' @include generics.R
#' @examples
#' S = struct_class()

struct_class<-setClass(
    "struct_class",
    slots = c(name = 'character',
        description = "character",
        type = "character",
        libraries = 'character'
    )
)

setMethod('initialize','struct_class',function(.Object,...) {

    L = list(...)
    SN = slotNames(.Object)
    if (length(L)>0)
    {
        for (i in seq_len(length(L)))
        {
            if (names(L)[[i]] %in% SN) {
                slot(.Object,names(L)[[i]]) = L[[names(L)[[i]]]]
            } else if (is(.Object,'parameter_class')) {
                param_value(.Object,names(L)[[i]]) = L[[names(L)[[i]]]]
            } else {
                stop(paste0(names(L)[[i]], 'is not a valid for ', class(.Object), ' objects.'))
            }

        }
    }

    # check if packages are available
    not_found = character(0)
    for (k in .Object@libraries) {
        if (!requireNamespace(k, quietly = TRUE)) {
            not_found = c(not_found,k)
        }
    }

    if (length(not_found)>0) {
        stop(paste0('The following packages are required but not installed: ', paste0(not_found,collapse = ', ',
            '. Please install them.')),
            call. = FALSE)
    }

    return(.Object)

})


#' name
#'
#' get the name of an object
#' @rdname object_name
#' @export
#' @param obj a struct object
#' @param value value
#' @examples
#' S = struct_class()
#' name(S)
#' @return (long) name of struct object
setMethod(f = "name",
    signature = "struct_class",
    definition = function(obj)
    {
        return(obj@name)
    }
)

#' @rdname object_name
#' @export
#' @examples
#' S = struct_class()
#' name(S) = 'example'
#' @return modified struct object
setMethod(f = "name<-",
    signature = c("struct_class"),
    definition = function(obj,value)
    {
        obj@name<-value
        return(obj)
    }
)

#' description
#'
#' get the description of an object
#' @rdname object_desc
#' @export
#' @param obj a struct object
#' @param value value
#' @examples
#' S = struct_class()
#' description(S)
#' @return description assigned to the struct object
setMethod(f = "description",
    signature = "struct_class",
    definition = function(obj)
    {
        return(obj@description)
    }
)

#' @examples
#' S = struct_class()
#' description(S) = 'this is an example'
#' @return modified struct object
#' @rdname object_desc
#' @export
setMethod(f = "description<-",
    signature = c("struct_class"),
    definition = function(obj,value)
    {
        obj@description<-value
        return(obj)
    }
)

#' type
#'
#' get the type of an object
#' @export
#' @param obj a struct object
#' @examples
#' S = struct_class()
#' type(S)
#' @return the type assigned to the struct objects
setMethod(f = "type",
    signature = "struct_class",
    definition = function(obj)
    {
        return(obj@type)
    }
)

#' @describeIn type set the type of an object
#' @export
#' @examples
#' S = struct_class()
#' type(S) = 'example'
#' @return modified struct object
setMethod(f = "type<-",
    signature = c("struct_class"),
    definition = function(obj,value) {
        obj@type<-value
        return(obj)
    }
)

#' chart names
#'
#' print a list of chart objects associated with the input object
#' @param obj a chart object
#' @param ret a string indicating whether to return a list of chart
#' names (ret = "char") or a list of chart objects (ret = "obj").
#' The default is "char".
#' @export
#' @examples
#' S = struct_class()
#' chart_names(S)
#' chart_names(S,'char')
#' chart_names(S,'obj')
#' @return list of chart names (default), or chart objects
setMethod(f = "chart_names",
    signature = c("struct_class"),
    definition = function(obj,ret = 'char') {
        if (ret == 'char') {
            OUT = character(0)
        } else if (ret == 'obj') {
            OUT = list()
        } else {
            stop('not a valid ret option. Try "char" or "obj"')
        }
        x = showMethods(f = chart_plot,classes = class(obj)[1],printTo = FALSE)
        if (x[2] == '<No methods>') {
        } else {

            for (i in 2:length(x)) {
                a = strsplit(x[i],'\"')[[1]]
                if (length(a)>0) {
                    a = a[seq(2, length(a), by = 2)]
                    a = a[-which(a == class(obj)[1])]
                    if (length(a)>0) {
                        if (a == 'chart') {
                        } else if (extends(a,'chart')) {
                            if (ret == 'char') {
                                OUT = c(OUT,a)
                            } else
                                OUT[[length(OUT)+1]] = eval(parse(text = paste0(a,
                                    '()')))
                        }
                    }
                }
            }
        }
        return(OUT)
    }
)

setMethod(f = "show",
    signature = c("struct_class"),
    definition = function(object) {
        cat(
            'A "', class(object),'" object','\n',
            'name:          ', name(object),'\n',
            'description:   ', paste0(strwrap(description(object),width=65,exdent = 15),collapse='\n'),
            sep = ''
        )
        cat('\n')
    }
)


#' define a new struct object
#'
#' a helper function to create new struct objects
#' @export
#' @param class_name the name of the new class to create
#' @param struct_obj the struct obj to inherit e.g. 'model', 'metric' etc
#' @param stato TRUE (default) or FALSE to inherit the stato class
#' @param params a named character vector of input parameters where each
#' element specifies the type of value that will be in the slot e.g. c(example = 'character')
#' @param outputs a named character vector of outputs where each
#' element specifies the type of value that will be in the slot e.g. c(example = 'character')
#' @param private a named character vector of private slots where each
#' element specifies the type of value that will be in the slot e.g. c(example = 'character').
#' These are intended for internal use by the object and generally not available to the user.
#' @param prototype a named list with initial values for slots.
#' @return a new class definition. to create  a new object from this class use X = new_class_name()
set_struct_obj = function(
    class_name, 
    struct_obj, 
    stato = TRUE, 
    params = character(0), 
    outputs = character(0), 
    private = character(0), 
    prototype = list()) {

    # inherit stato if stato = TRUE
    if (stato) {
        struct_obj = c(struct_obj,'stato')
    }

    ## list of slots to create
    # adjust names for params and outputs
    np = names(params)
    npi = as.character(interaction('params',np,sep = '_'))
    names(params) = npi
    no = names(outputs)
    noi = as.character(interaction('outputs',no,sep = '_'))
    names(outputs) = noi
    # combine into a slot vector
    slots = c(params,outputs,private)

    ## change the names of any prototypes if they are listed in params or outputs
    w = which(names(prototype) %in% np)
    names(prototype)[w] = npi[w]
    w = which(names(prototype) %in% no)
    names(prototype)[w] = noi[w]

    ## create class definition as assign to the chosen environment

    assign(class_name,setClass(
        Class = class_name,
        contains = struct_obj,
        slots = slots,
        prototype = prototype,
        where = topenv(parent.frame())
    ),
    topenv(parent.frame()))



}


#' update method for a struct object
#'
#' a helper function to update methods for a struct object
#' @export
#' @param class_name the name of the to update the method for
#' @param method_name the name of the method to update. Must be an existing method for the object.
#' @param definition the function to replace the method with. This function will be used when the method is called on the object.
#' @param where the environment to create the object in. default where = topenv(parent.frame())
#' @return a method is created in the specified environment
#' @examples
#' set_struct_obj(
#' class_name = 'add_two_inputs',
#' struct_obj = 'model',
#' stato = FALSE,
#' params = c(input_1 = 'numeric', input_2 = 'numeric'),
#' outputs = c(result = 'numeric'),
#' prototype = list(
#'    input_1 = 0,
#'    input_2 = 0,
#'    name = 'Add two inputs',
#'    description = 'example class that adds two values together')
#')
set_obj_method = function(class_name, method_name, definition, where = topenv(parent.frame())) {

    setMethod(f = method_name,
        signature = c(class_name,'dataset'),
        definition = definition,
        where = where
    )
}

# ' update show method for a struct object
#'
#' a helper function to update the show method for a struct object
#' @export
#' @param class_name the name of the to update the method for
#' @param extra_string a function that returns an extra string using the input object as an input e.g. function(object){return = 'extra_string'}
#' @param where the environment to create the object in. default where = topenv(parent.frame())
#' @return a method is created in the specified environment
#' @examples
#' # create an example object first
#' set_struct_obj(
#' class_name = 'add_two_inputs',
#' struct_obj = 'model',
#' stato = FALSE,
#' params = c(input_1 = 'numeric', input_2 = 'numeric'),
#' outputs = c(result = 'numeric'),
#' prototype = list(
#'    input_1 = 0,
#'    input_2 = 0,
#'    name = 'Add two inputs',
#'    description = 'example class that adds two values together')
#')
#'
#' # now update the method
#' set_obj_show(
#' class_name = 'add_two_inputs',
#' extra_string = function(object) {return('The extra text')}
#' )
#'
set_obj_show = function(class_name, extra_string,where = topenv(parent.frame())) {

    setMethod(f = 'show',
        signature = c(class_name),
        definition = function(object) {
            callNextMethod() # force the default output
            # add extra info
            cat(extra_string(object))
        },
        where = where
    )
}
