#' \code{struct_class} object definition
#'
#' Defines the struct class base template. This class is inherited by other objects
#' and not intended for direct use. It defines slots and methods common to all
#' \pkg{struct} objects.
#' 
#' @section Public slots:
#' Public slots can be accessed using shorthand $ notation and are intended for 
#' users building workflows.
#'  
#' \describe{
#'   \item{\code{name}}{\code{character()} A short descriptive name of the struct object}
#'   \item{\code{description}}{\code{character()} A longer description of the struct object and what it does}
#'   \item{\code{type}}{\code{character()} A keyword that describes the type of struct object}
#'   \item{\code{libraries}}{\code{character()} A (read only) list of R packages used by this struct object}
#'   \item{\code{citations}}{\code{list of bibentry} A (read only) list of citations relevant to this struct object,
#'   in Bibtex format.}
#' }
#' 
#' @section Private slots:
#' Private slots are not readily accessible to users and are intended for developers
#' creating their own struct objects. Any slot not listed within `.params` or 
#' `.outputs` is considered a private slot.
#' 
#' \describe{
#'   \item{\code{.params}}{\code{character()} A list of additional slot names that can be get/set by the user
#'   for a specific struct object. These are used as input parameters for different methods.}
#'   \item{\code{.outputs}}{\code{character()} a list of additional slot names that can be get by the user. These are
#'   used to store the results of a method.}
#' }
#' 
#' 
#' @import methods
#' @include generics.R ontology_term_class.R
#' @return Returns a \pkg{struct} object
#' @examples
#' S = struct_class(name = 'Example',description = 'An example object')
#' @export
.struct_class<-setClass(
    "struct_class",
    slots = c(
        name = 'character',
        description = "character",
        type = "character",
        libraries = 'character',
        citations = 'list',
        ontology = 'character',
        .params='character',
        .outputs='character'
    ),
    prototype = list(
        'citations'=suppressWarnings(list(citation('struct')))
    )
)

#' Constructor for struct_class objects
#' 
#' Creates a new \linkS4class{struct_class} object and populates the slots. Not intended
#' for direct use.
#' @param name the name of the object
#' @param description a description of the object
#' @param type the type of the struct object
#' @param citations a list of citations for the object in "bibentry" format
#' @param ontology a list of ontology items for the object in "ontology_item" format
#' @return a struct_class object
#' @export
struct_class = function(
    name=character(0),
    description=character(0),
    type=character(0),
    citations=list(),
    ontology=character(0)) {
    
    # if Bibtex is provided convert to a list
    if (is(citations,'bibentry')){
        citations=list(citations)
    }
    
    # check all citations are Bibtex
    if (length(citations>0)) {
        ok=unlist(lapply(citations,is,class='bibentry'))
        if (!(all(ok))){
            stop('all citations must be in "bibentry" format')
        }
    }
    
    # new object
    out = .struct_class(
        name=name,
        description=description,
        type=type,
        citations=citations,
        ontology=ontology
    )
    
    return(out)
}

setValidity('struct_class', method = function(object) {
    str = character(0)
    
    # check for libraries
    check = lapply(object$libraries,function(x){
        length(find.package(x,quiet=TRUE))>0
    })
    
    check=unlist(check)
    
    if (length(check)>0) {
        w=which(!check)
        if (length(w)==1) {
            str=paste0(
                'The required package "', object$libraries[w], '" is not installed.',
                collapse = ','
            )
        } else if (length(w)>1) {
            str=paste0(
                'The required packages c(', paste0('"',object$libraries[w],'"'), ') are not installed.',
                collapse = ','
            )
        }
    }
    
    citations=object$citations
    # check all citations are Bibtex
    if (length(citations>0)) {
      ok=unlist(lapply(citations,is,class='bibentry'))
      if (!(all(ok))){
        str=c(str,('All citations must be in "bibentry" format'))
      }
    }
    
    if (length(str)>0)
        return(str)
    else
        return(TRUE)
}
    
)

#' Get/set parameter or output values
#' 
#' Dollar syntax can be used to as a shortcut for getting/setting input parameter 
#' and output values for struct objects.
#' @return Parameter/output value
#' @param x An object derived from struct_class
#' @param name The name of the slot to access
#' @examples 
#' M = example_model()
#' M$value_1 = 10
#' M$value_1 # 10
#' @export
setMethod(f = "$",
    signature = c("struct_class"),
    definition = function(x,name) {
        
        # check for param
        w = is_param(x,name)
        if (w) {
            out = param_value(x,name)
            return(out)
        }
        
        # check for output
        w = is_output(x,name)
        if (w) {
            out = output_value(x,name)
            return(out)
        }
        
        # check for other struct slots
        valid=c('name','description','type','libraries','citations','ontology')
        if (name %in% valid) {
            out = slot(x,name)
            return(out)
        }
        
        # if we get here then error
        stop('"', name, '" is not valid for this object: ', class(x)[1])
        
    }
)

#' Get/set parameter or output values
#' 
#' Dollar syntax can be used to as a shortcut for getting/setting input parameter 
#' and output values for struct objects.
#' @return Parameter/output value
#' @param x An object derived from struct_class
#' @param name The name of the slot to access
#' @param value The value to assign
#' @examples 
#' M = example_model()
#' M$value_1 = 10
#' M$value_1 # 10
#' @export
setMethod(f = "$<-",
    signature = c("struct_class"),
    definition = function(x,name,value) {
        
        
        # check for param
        if (is_param(x,name)) {
            param_value(x,name) = value
            return(x)
        }
        
        # check for output
        if (is_output(x,name)) {
            output_value(x,name) = value
            return(x)
        }
        
        # check for other slots
        valid=c('name','description','type') 
        # do not allow setting of libraries or citations or ontology by user
        if (name %in% valid) {
            slot(x,name) = value
            return(x)
        } 
        
        # if we havent returned value by now, then we're not going to
        stop(name,' is not a valid param, output or column name for ',
            'this DatasetExperiment using $')
        
    }
)

#' @describeIn chart_names 
#' @export
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
        n=nchar(paste0('A "', class(object),'" object'))
        
        if (length(object@description) > 1) {

            nmes=names(object$description)
            if (is.null(nmes)) {
                # add bullets to description if more than one item
                object@description=paste0('\U2022',' ', object$description)
            } else {
                nmes=paste0(nmes,':')
                padding=max(nchar(nmes))
                padding=strrep(' ',padding)
                
                for (k in seq_along(nmes)) {
                    nme=sub(strrep(' ',nchar(nmes[k])),nmes[k],padding)
                    object@description[k]=paste0(nme,' ',object@description[k])
                }
                # add name to description if more than one item
                object@description=paste0('\U2022',' ', object$description)
            }
        }
        # strip newlines from description, we'll add our own
        object@description=gsub("[\r\n]",'',object@description)
        if (length(object$description)>1) {
            pad='\n               '
        } else {
            pad='\n'
        }

        cat(
            'A "', class(object),'" object','\n',
            rep('-',n),'\n',
            'name:          ', paste0(strwrap(object$name,width=95,exdent = 17),collapse=pad),'\n',
            'description:   ', paste0(strwrap(object$description,width=95,exdent = 17),collapse=pad),'\n',
            sep = ''
        )
        
        if (length(object@.params>0) & !is(object,'entity')) {
            cat('input params: ', paste(object@.params,collapse=', '),'\n')
        } 
        if (length(object@.outputs>0) & !is(object,'entity')) {
            cat('outputs:      ', paste(object@.outputs,collapse=', '),'\n')
        } 
        
    }
)


#' define a new struct object
#'
#' a helper function to create new struct objects
#' @export
#' @param class_name the name of the new class to create
#' @param struct_obj the struct obj to inherit e.g. 'model', 'metric' etc
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
    params = character(0), 
    outputs = character(0), 
    private = character(0), 
    prototype = list()) {
    
    ## list of slots to create
    slots = c(params,outputs,private)
    
    ## add .params and .outputs to prototype
    prototype[['.params']]=names(params)
    prototype[['.outputs']]=names(outputs)
    
    ## create class definition and assign to the chosen environment
    
    assign(paste0('.',class_name),setClass(
        Class = class_name,
        contains = struct_obj,
        slots = slots,
        prototype = prototype,
        where = topenv(parent.frame())
    ),
        topenv(parent.frame()))
    
    assign(class_name,function(...){
        # new object
        out = eval(parse(text=paste0('new_struct("',class_name,'",...)')))
        return(out)
    },
        topenv(parent.frame())
    )
    
}


#' update method for a struct object
#'
#' a helper function to update methods for a struct object
#' @export
#' @param class_name the name of the to update the method for
#' @param method_name the name of the method to update. Must be an existing method for the object.
#' @param definition the function to replace the method with. This function will be used when the method is called on the object.
#' @param where the environment to create the object in. default where = topenv(parent.frame())
#' @param signature a list of classes that this object requires as inputs. Default is c(class_name,'DatasetExperiment')
#' @return a method is created in the specified environment
#' @examples
#' set_struct_obj(
#' class_name = 'add_two_inputs',
#' struct_obj = 'model',
#' params = c(input_1 = 'numeric', input_2 = 'numeric'),
#' outputs = c(result = 'numeric'),
#' prototype = list(
#'    input_1 = 0,
#'    input_2 = 0,
#'    name = 'Add two inputs',
#'    description = 'example class that adds two values together')
#')
set_obj_method = function(class_name, method_name, definition, where = topenv(parent.frame()), signature=c(class_name,'DatasetExperiment')) {
    
    setMethod(f = method_name,
        signature = signature,
        definition = definition,
        where = where)
    
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

populate_slots=function(obj,...) {
    L=list(...)
    for (k in L) {
        if (is_param(obj,names(k))) {
            param_value(obj,names(k)) = k[[1]]
        }
        
    }
}


#' Generate a \pkg{struct} object from a Class
#' 
#' This function creates a newly allocated object from the class identified by 
#' the first argument. It works almost identically to \code{new} but is specific 
#' to objects from the \pkg{struct} package and ensures that \code{entity} slots have 
#' their values assigned correctly. This function is usually called by class 
#' constructors and not used directly.
#' 
#' @param class The class of struct object to create
#' @param ... named slots and values to assign
#' @return An object derived from struct_class
#' @examples
#' S = new_struct('struct_class')
#' @export
new_struct = function(class, ...) {
    
    # new default object
    obj=new(class)
    
    # check if struct_class
    if (!is(obj,'struct_class')){
        stop('struct_class is only for objects derived from struct_class. Got object of type "',class(obj),'"')
    }
    
    # convert to entity if req
    L=list(...)
    for (k in seq_len(length(L))) {
        isEntity = is(param_obj(obj,names(L[k])),'entity')
        
        # if entity, replace input value with entity and assign value
        if (isEntity) {
            ent = param_obj(obj,names(L[k]))
            value(ent) = L[[k]]
            L[[k]]=ent
        }
    }
    
    L$Class = class
    obj = do.call(new,L) # will check for validity
    
    return(obj)
}



#' @rdname citations
#' @importFrom utils capture.output bibentry as.person citation
#' @export
setMethod(f = "citations",
    signature = c("struct_class"),
    definition = function(obj) {
        if (is(obj,'DatasetExperiment')) {
            cit=obj$citations
        } else {
            cit=list()
        }
        
        # citations for libraries
        lib = .extended_list_by_slot(obj,'libraries')
            lib = lapply(lib,function(x){
                # citations for library
                A = suppressWarnings(citation(x))
                # convert to strings
                #B=.list_of_citations_as_strings(A)
                return(A)
            })
        
        cit = c(cit,lib)
        
        # citations as strings
        out = .extended_list_by_slot(obj,'citations')
        cit=c(cit,out)
        
        # remove duplicates
        cit=cit[!(duplicated(cit))]
        return(cit)
    }
)

#' @rdname libraries
#' @export
setMethod(f = "libraries",
    signature = c("struct_class"),
    definition = function(obj) {
        lib=.extended_list_by_slot(obj,'libraries')
        lib=lib[!(duplicated(lib))]
        return(lib)
    }
)




.extended_list_by_slot = function(obj,slotname) {
    # returns a unique list of values for slots in this object
    # and all the ones in inherits
    cit=slot(obj,slotname)
    # get the objects this object extends
    ex = extends(class(obj)[1])
    # for each one, if its a struct class grab the citations
    for (k in seq_along(ex)) {
        if (extends(ex[[k]],'struct_class')) {
            X = new_struct(ex[k])
            S=slot(X,slotname)
            cit=c(cit,S)
        }
    }
    return(cit)
}


#' @rdname ontology
#' @export
setMethod(f = "ontology",
    signature = c("struct_class"),
    definition = function(obj,cache=NULL) {
        
        # ontology for object and inherited
        ont = .extended_list_by_slot(obj,'ontology')
        
        # ontology for params and outputs
        p=param_ids(obj)
        pont=lapply(p,function(x){
            ent=param_obj(obj,x)
            if (is(ent,'struct_class')) {
                return(ent$ontology)
            } else {
                return(character(0))
            }
        })
        o=output_ids(obj)
        oont=lapply(o,function(x){
            ent=output_obj(obj,x)
            if (is(ent,'struct_class')) {
                return(ent$ontology)
            } else {
                return(character(0))
            }
        })
        
        ont = c(ont,unlist(pont),unlist(oont))
        
        # remove duplicates
        ont=ont[!(duplicated(ont))]
        
        # get definitions
        if (!is.null(cache)) {
            # use cache
            ont=lapply(ont,function(x){
                ontology_list(cache[[x]])
            })
        } else {
            # use api
            ont=ontology_list(ont)
        }
    
        return(ont)
    }
)


#' autocompletion
#' 
#' This function returns slotnames for autocompletion when using $ syntax
#' @param x a struct_class object
#' @param pattern the text used to compare against the slot names
#' @return A vector of slot names
#' @rdname autocompletion
#' @export
#' @method .DollarNames struct_class
.DollarNames.struct_class <- function(x, pattern = "") {
    s = slotNames(x)
    IN = s %in% c(
        slot(x,'.params'),
        slot(x,'.outputs'),
        c('name','description','type','libraries','citations','ontology')
    )
    s=s[IN]
    return(grep(pattern, s, value=TRUE))
}

#' @export 
#' @rdname autocompletion
setMethod('.DollarNames','struct_class',.DollarNames.struct_class)
