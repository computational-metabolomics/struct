#' stato_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental methods for the STATO ontology and should not be called directly.
#' @export stato
#' @param obj stato_class object
#' @include generics.R
#' @importFrom ontologyIndex get_ontology
#' @examples
#' S = stato()

stato<-setClass(
    "stato",
    slots=c('stato.id'="character"

    )
)

#' @describeIn stato get the stato.id for an object
#' @export
#' @examples
#' \dontrun{
#' M = model.stato()
#' stato.id(M)
#' }
#' @return stato id
setMethod(f="stato.id",
    signature=c('stato'),
    definition=function(obj)
    {
        if (!exists('ont',envir = statoOntology))
        {
            # load the ontology if it hasnt been done already
            stato.env()
        }
        return(obj@stato.id)
    }
)

#' @describeIn stato get the STATO name for an object
#' @export
#' @examples
#' \dontrun{
#' M = model()
#' stato.name(M)
#' }
#' @return stato name
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
#' @examples
#' \dontrun{
#' M = model()
#' stato.definition(M)
#' }
#' @return stato definition
setMethod(f="stato.definition",
    signature=c('stato'),
    definition=function(obj)
    {
        id=stato.id(obj)
        return(strip_special(statoOntology$ont$def[[id]]))
    }
)

statoOntology=new.env()

stato.env=function()
{
    path.to.ontology=file.path(path.package('struct'),'/extdata/stato-reasoned.obo')
    assign('ont',ontologyIndex::get_ontology(path.to.ontology,extract_tags = 'everything'),envir=statoOntology)
}

#' @describeIn stato get the STATO definition for an object
#' @export
#' @examples
#' \dontrun{
#' M = model()
#'stato.summary(M,'example')
#' }
#' @return summary of the stato object
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
        #                        cat('\nCharts:\n')
        #                        p=chart.ids(obj)
        #                        for (i in p)
        #                        {
        #                            if (is(chart.obj(obj,i),'stato'))
        #                            {
        #                                cat(stato.id(chart.obj(obj,i)),'\n')
        #                                cat(stato.name(chart.obj(obj,i)),'\n')
        #                                cat(stato.definition(chart.obj(obj,i)),'\n\n')
        #                            }
        #                        }
    }
)

# internal function to strip special chars from the description
strip_special=function(str,chars="\"|\\[|\\]")
{
    str=gsub(pattern=chars, replacement="", x=str)
    return(str)
}


