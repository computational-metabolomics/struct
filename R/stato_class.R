#' stato_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental
#' methods and should not be called directly.
#'
#' STATO is the statistical methods ontology. It contains concepts and
#' properties related to statistical methods, probability distributions and
#' other concepts related to statistical analysis, including relationships to
#' study designs and plots (see \href{http://stato-ontology.org/}{
#' http://stato-ontology.org/}).
#'
#' This class provides access to a version of the STATO ontology database that
#' can be searched by ontology id to provide formal names and definitions for
#' methods, models, iterators, metrics and charts.
#'
#' This class makes use of the \code{ontologyIndex} package to search a copy of
#' the STATO database included in this package.
#'
#' @export stato
#' @param obj An object derived from the stato object
#' @param stato_id A STATO ID e.g. OBI:0000001
#' @include generics.R
#' @importFrom ontologyIndex get_ontology
#' @return Value returned depends on the method used.
#' @examples
#' # an example stato object
#' M = example_model()
#'
#' # the stato id assigned to object M
#' stato_id(M) # OBI:0000011
#'
#' # the name associated with that id
#' stato_name(M)
#'
#' # the STATO definition for that id
#' stato_definition(M)
#'
#' # a summary of the STATO database entry for the id, and any parameters or
#' # outputs that also have stato ids.
#' stato_summary(M)
#'
#' @rdname stato
stato = function(stato_id) {
    # new object
    out = .stato(stato_id=stato_id)
    return(out)
}

.stato<-setClass(
    "stato",
    slots = c('stato_id' = "character")
)

#' @rdname stato
#' @export
setMethod(f = "stato_id",
    signature = c('stato'),
    definition = function(obj) {
        if (!exists('ont',envir = statoOntology)) {
            # load the ontology if it hasn't been done already
            .stato_env()
        }
        return(obj@stato_id)
    }
)

#' @rdname stato
#' @export
setMethod(f = "stato_name",
    signature = c('stato'),
    definition = function(obj) {
        # get the stato id
        id = stato_id(obj)
        # get the name from the stato database
        nme = statoOntology$ont$name[[id]]
        return(nme)
    }
)

#' @rdname stato
#' @export
setMethod(f = "stato_definition",
    signature = c('stato'),
    definition = function(obj) {
        # get the id for the object
        id = stato_id(obj)
        # get the definition and clean any special chars
        id = .strip_special(statoOntology$ont$def[[id]])
        return(id)
    }
)

# create a new environment for the stato database
statoOntology = new.env()

# internal function to extract the database into the environment
.stato_env = function() {
    path.to.ontology = system.file(package='struct','extdata/stato-reasoned.obo')
    
    assign('ont',
        ontologyIndex::get_ontology(path.to.ontology,
            extract_tags = 'everything'),
        envir = statoOntology
    )
}

#' @rdname stato
#' @export
setMethod(f = "stato_summary",
    signature = c('stato'),
    definition = function(obj) {
        cat(stato_id(obj),'\n')
        cat(stato_name(obj),'\n')
        cat(stato_definition(obj),'\n')
        cat('\nInputs:\n')
        
        p = param_ids(obj)
        for (i in p) {
            if (is(param_obj(obj,i),'stato')) {
                cat(stato_id(param_obj(obj,i)),'\n')
                cat(stato_name(param_obj(obj,i)),'\n')
                cat(stato_definition(param_obj(obj,i)),'\n\n')
            }
        }
        cat('\nOutputs:\n')
        p = output_ids(obj)
        for (i in p) {
            if (is(output_obj(obj,i),'stato')) {
                cat(stato_id(output_obj(obj,i)),'\n')
                cat(stato_name(output_obj(obj,i)),'\n')
                cat(stato_definition(output_obj(obj,i)),'\n\n')
            }
        }
        
    }
)

# internal function to strip special chars from the description
.strip_special = function(str,chars = "\"|\\[|\\]") {
    str = gsub(pattern = chars, replacement = "", x = str)
    return(str)
}


setMethod(f = 'show',
    signature = c('stato'),
    definition = function(object) {
        # add extra info
        cat('Stato ID:      ',stato_id(object),sep='')
        cat('\n')
        
    }
)