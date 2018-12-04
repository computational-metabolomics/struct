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
#' @param obj stato_class object
#' @include generics.R
#' @importFrom ontologyIndex get_ontology
#' @return Value returned depends on the method used.
#' @examples
#' # an example stato object
#' M = example_model()
#'
#' # the stato id assigned to object M
#' stato.id(M) # OBI:0000011
#'
#' # the name associated with that id
#' stato.name(M)
#'
#' # the STATO definition for that id
#' stato.definition(M)
#'
#' # a summary of the STATO database entry for the id, and any parameters or
#' # outputs that also have stato ids.
#' stato.summary(M)
#'
stato<-setClass(
    "stato",
    slots=c('stato.id'="character"

    )
)

#' @describeIn stato get the stato.id for an object
#' @export
setMethod(f="stato.id",
    signature=c('stato'),
    definition=function(obj)
    {
        if (!exists('ont',envir = statoOntology))
        {
            # load the ontology if it hasnt been done already
            .stato.env()
        }
        return(obj@stato.id)
    }
)

#' @describeIn stato get the STATO name for an object
#' @export
setMethod(f="stato.name",
    signature=c('stato'),
    definition=function(obj)
    {
        id=stato.id(obj)
        return(statoOntology$ont$name[[id]])
    }
)

#' @describeIn stato get the STATO definition for an object
#' @export
setMethod(f="stato.definition",
    signature=c('stato'),
    definition=function(obj)
    {
        id=stato.id(obj)
        return(.strip_special(statoOntology$ont$def[[id]]))
    }
)

# create a new environment for the stato database
statoOntology=new.env()

# internal function to extract the database into the environment
.stato.env=function()
{
    path.to.ontology=file.path(path.package('struct'),
        '/extdata/stato-reasoned.obo')
    assign('ont',ontologyIndex::get_ontology(path.to.ontology,
        extract_tags = 'everything'),
        envir=statoOntology)
}

#' @describeIn stato get the STATO definition for an object
#' @export
setMethod(f="stato.summary",
    signature=c('stato'),
    definition=function(obj)
    {
        cat(stato.id(obj),'\n')
        cat(stato.name(obj),'\n')
        cat(stato.definition(obj),'\n')
        cat('\nInputs:\n')
        p=param.ids(obj)
        for (i in p)
        {
            if (is(param.obj(obj,i),'stato'))
            {
                cat(stato.id(param.obj(obj,i)),'\n')
                cat(stato.name(param.obj(obj,i)),'\n')
                cat(stato.definition(param.obj(obj,i)),'\n\n')
            }
        }
        cat('\nOutputs:\n')
        p=output.ids(obj)
        for (i in p)
        {
            if (is(output.obj(obj,i),'stato'))
            {
                cat(stato.id(output.obj(obj,i)),'\n')
                cat(stato.name(output.obj(obj,i)),'\n')
                cat(stato.definition(output.obj(obj,i)),'\n\n')
            }
        }

    }
)

# internal function to strip special chars from the description
.strip_special=function(str,chars="\"|\\[|\\]")
{
    str=gsub(pattern=chars, replacement="", x=str)
    return(str)
}


