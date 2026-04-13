#' Query the OLS API for ontology term information
#'
#' Internal helper function to fetch term details from the Ontology Lookup
#' Service API.
#'
#' @param obo_id (character) The ontology ID in format PREFIX:DIGITS
#' (e.g., 'STATO:0000046')
#' @param api_url (character) The base URL for the OLS API. Default is the
#' current OLS4 API endpoint. Can be overridden if the API URL changes or
#' for testing purposes.
#'
#' @return A list with elements:
#' \describe{
#'   \item{label}{The term label}
#'   \item{description}{The term description}
#'   \item{iri}{The Internationalized Resource Identifier}
#' }
#' Returns NULL if the query fails.
#'
#' @keywords internal
#' @import jsonlite
#' @import httr2
#' @importFrom utils URLencode
.query_ols_api = function(
        obo_id,
        api_url = "https://www.ebi.ac.uk/ols4/api"
) {
    tryCatch({

        # early exit if offline is set
        if (isFALSE(getOption("struct.ontology.online", TRUE))) {
            return(NULL)
        }

        # get ontology from id
        parts = strsplit(obo_id, ':')[[1]]
        ontology_prefix = tolower(parts[1])
        term_id = obo_id

        # Construct direct lookup URL
        url = sprintf("%s/ontologies/%s/terms?obo_id=%s",
                      api_url,
                      ontology_prefix,
                      URLencode(term_id, reserved = TRUE))

        response = jsonlite::fromJSON(url,simplifyDataFrame = FALSE)

        terms = response$`_embedded`$terms[[1]]

        # Direct lookup returns a single term
        list(
            label = if (!is.null(terms$label)) terms$label else character(),
            description = if (!is.null(terms$description)) terms$description else character(),
            iri = if (!is.null(terms$iri)) terms$iri else character()
        )
    }, error = function(e) {
        warning("Could not fetch ontology term from OLS API for ID: ", obo_id,
                ". Error: ", e$message)
        NULL
    })
}


#' ontology_term object
#'
#' A base class in the \pkg{struct} package. Stores
#' ontology information e.g. term, description, id etc for struct objects
#' and provides methods for populating these fields using the
#' `rols` package.
#'
#' @export ontology_term
#' @param id (character) The ontology term id e.g. 'STATO:0000046'
#' @include generics.R
#' @examples
#' \dontrun{
#' OT = ontology_term(id='STATO:0000046')
#' }
#' @rdname ontology
ontology_term = function(
        id
) {

    # check the id follows the expected pattern
    # any number of uppercase chars, colon separated, any number of numbers
    pattern = "^[A-Z]+:[0-9]+$"
    check = grepl(pattern, id)

    if (!check) {
        stop(
            "Invalid ontology ID format. Expected format: PREFIX:DIGITS ",
            "(e.g., STATO:0000046)")
    }

    # Extract ontology prefix if not provided
    ontology = tolower(sub(':.*', '', id))  # Get prefix before colon

    # Query OLS API with the obo_id
    result = .query_ols_api(id)

    if (is.null(result)) {
        # no result, so 0 length chars
        label = character()
        description = character()
        iri = character()
    } else {
        # result, so use for object
        label = result$label
        description = result$description
        iri = result$iri
    }

    # new object
    out = .ontology_term(
        id = id,
        label = label,
        description = description,
        ontology = ontology,
        iri = iri)

    return(out)
}

.ontology_term<-setClass(
    "ontology_term",
    slots = c(
        id = "character",
        label = 'character',
        description='character',
        ontology='character',
        iri='character'
    )
)

.ontology_slot = function(x,name) {

    if (!(name %in% slotNames(x))) {
        stop('"',name,'" is not a valid slot name for ',
             class(x)[1],'  objects.')
    }

    return(slot(x,name))

}

#' Get/set ontology term slots
#'
#' Dollar syntax can be used to as a shortcut for getting
#' values for ontology_term objects.
#' @return Slot value
#' @param x An ontology_term object
#' @param name The name of the slot to access
#' @examples
#' \dontrun{
#' OT = ontology_term(id='STATO:0000046')
#' OT$ontology # returns "STATO"
#' }
#' @export
setMethod(f = "$",
          signature = c("ontology_term"),
          definition=function(x,name){
              return(.ontology_slot(x,name))
          }
)

setMethod(f = 'show',
          signature = c('ontology_term'),
          definition = function(object) {

              # report if offline
              if (isFALSE(getOption("struct.ontology.online", TRUE))) {
                  cat("--OLS API lookup is currently disabled.--\n")
              } else {

                  # padding for long descriptions and names
                  pad = '\n               '
                  desc=paste0(strwrap(object$description,width=95,exdent = 2),
                              collapse=pad)
                  label=paste0(strwrap(object$label,width=95,exdent = 2),
                               collapse=pad)
                  cat('term id:       ',object$id,'\n',sep='')
                  cat('ontology:      ',object$ontology,'\n',sep='')
                  cat('label:         ',label,'\n',sep='')
                  cat('description:   ',desc,'\n',sep='')
                  cat('iri:           ',object$iri,'\n',sep='')
              }
          }
)


#' ontology_list object
#'
#' A base class in the \pkg{struct} package. Stores
#' multiple `ontology_term` objects.
#'
#' @export ontology_list
#' @param ... character ontology IDs (e.g. STATO:0000046) or NULL for an empty list
#' @include generics.R
#' @examples
#' \dontrun{
#' OT = ontology_list(terms=list(
#'     ontology_term(id =  'OBI:0200051'),
#'     ontology_term(id ='STATO:0000046')
#' )
#' }
#' @rdname ontology
ontology_list = function(...) {

    # collect ...
    terms = list(...)

    # check all char
    check = all(unlist(lapply(terms,is.character)))
    if (!check) {
        stop(
            'All terms must be character conforming to the ontology id ',
            'format: PREFIX:DIGITS e.g. STATO:0000046')
    }

    # combine
    terms = do.call('c',terms)

    # if no terms then empty list
    if (length(terms)==0) {
        return(.ontology_list(terms=list()))
    }

    # convert all terms to ontology items
    # NB this also checks expected format for terms
    if (length(terms)>0) {
        terms = lapply(terms,ontology_term)
    }

    # new object
    out = .ontology_list(
        terms = terms)
    return(out)
}

.ontology_list<-setClass(
    "ontology_list",
    slots = c(
        terms='list'
    )
)

#' Get/set ontology_list slots
#'
#' Dollar syntax can be used to as a shortcut for getting
#' values for ontology_list objects.
#' @return Slot value
#' @param x An ontology_term object
#' @param name The name of the slot to access
#' @examples
#' \dontrun{
#' OL = ontology_list('STATO:0000046')
#' OL$terms
#' }
#' @export
setMethod(f = "$",
          signature = c("ontology_list"),
          definition = function(x,name){
              return(.ontology_slot(x,name))
          }
)

#' @rdname ontology
#' @export
#' @examples
#' \dontrun{
#' OL = ontology_list('STATO:0000046')
#' OL[1] # an ontology list
#' }
#'
#' @return \code{x[i]} returns a subset of list \code{x}
setMethod(f = "[",
          signature = "ontology_list",
          definition = function(x, i) {
              new_terms = x@terms[i]
              # create class directly; not using constructor
              .ontology_list(terms = new_terms)
          }
)

#' @rdname ontology
#' @export
#' @examples
#' \dontrun{
#' OL = ontology_list('STATO:0000046')
#' OL[[1]] # an ontology_item
#' }
#'
#' @return \code{x[[i]]} returns the ontology_term at index \code{i}.
setMethod(f = "[[",
          signature = "ontology_list",
          definition = function(x, i) {
              new_terms = x@terms[[i]]
          }
)

#' @rdname ontology
#' @param x the list
#' @param i The list item index
#' @param value an ontology_term() object
#' @export
#' @examples
#' \dontrun{
#' OL = ontology_list('STATO:0000046')
#' OL[1] = ontology_term('STATO:0000302')
#' }
#' @return \code{x[i] <-} replaces the list item at index \code{i}
setMethod(f = "[<-",
          signature = "ontology_list",
          definition = function(x,i,value) {

              # Convert character to ontology_term if needed
              if (is.character(value)) {
                  value = ontology_term(id = value)
              }

              if (!is(value,'ontology_term')) {
                  stop('value must be an ontology_term or character ontology id')
              }

              x@terms[[i]] = value

              return(x)
          }
)

#' @rdname ontology
#' @export
#' @examples
#' \dontrun{
#' OL = ontology_list()
#' length(OL) # 0
#' }
#' @return \code{length(x)} returns the number of items in the list.
setMethod(f = 'length',
          signature = 'ontology_list',
          definition = function(x) {
              return(length(x@terms))
          }
)



#' Enable or disable Ontology Lookup Service (OLS) requests
#'
#' Sets the package option \code{struct.ontology.online}. When \code{FALSE},
#' \code{\link{ontology_term}}, \code{\link{ontology_list}}, and
#' \code{\link{ontology}} do not perform HTTP requests to OLS; ontology IDs are
#' still stored, but label, description, and IRI are left empty (the same
#' outcome as a failed lookup).
#'
#' @param online Logical scalar. If \code{TRUE} (default when the option is
#'   unset), OLS is queried for term metadata. If \code{FALSE}, lookups are
#'   skipped and no network access is attempted.
#'
#' @return \code{online}, invisibly.
#'
#' @details
#' The setting is \code{options(struct.ontology.online = online)}. If the
#' option has never been set, behaviour matches \code{online = TRUE}.
#' Typical uses include vignette builds, unit tests, and offline sessions.
#'
#' @seealso \code{\link{ontology_term}}, \code{\link{ontology_list}},
#'   \code{\link{ontology}}
#'
#' @examples
#' ontology_online(FALSE)
#' ontology_online(TRUE)
#'
#' @export
ontology_online = function(online = TRUE) {
    options(struct.ontology.online = online)
    invisible(online)
}

setMethod("show", "ontology_list", function(object) {

    n <- length(object@terms)

    if (n == 0L) {
        cat("An \"ontology_list\" with 0 terms\n")
        return(invisible(object))
    } else {
        cat("An \"ontology_list\" with ",n," terms\n")
    }

    if (getOption("struct.ontology.online")) {
        show(object@terms)
    } else {
        terms=unlist(lapply(object@terms,function(x){
            return(x$id)
        }))
        cat(terms,sep='\n')
    }

    if (isFALSE(getOption("struct.ontology.online", TRUE))) {
        cat("--OLS API lookup is currently disabled.--\n")
    }

    invisible(object)
})

