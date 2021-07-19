#' ontology_term object
#'
#' A base class in the \pkg{struct} package. Stores
#' ontology information e.g. term, description, id etc for struct objects
#' and provides methods for populating these fields using the
#' `rols` package.
#'
#' @export ontology_term
#' @param id (character) The ontology term id e.g. 'STATO:0000555'
#' @param ontology (character) The ontology the term is a member of e.g. 'stato'
#' @param label (character) The label for the ontology term
#' @param description (character) The description of the term
#' @param iri (character) The Internationalized Resource Identifier for the term
#' @param rols (logical) TRUE or FALSE to query the Ontology Lookup Service for
#' missing label, description or iri if not provided as input. 
#' Default rols = TRUE
#' @include generics.R
#' @examples
#' \dontrun{
#' OT = ontology_term(id='STATO:0000555')
#' }
#' @rdname ontology
ontology_term = function(
    id,
    ontology=character(),
    label=character(),
    description=character(),
    iri=character(),
    rols=TRUE
) {
    
    # ensure we have a colon separator
    id=gsub('[ :_-]',id,replacement = ':',perl=TRUE)
    
    if (length(ontology)==0){
        # split the id and use that as basis for ontology
        ontology=regmatches(id,regexpr('^[^: _-]+[^: _-]',id))
        ontology=tolower(ontology)
    }
    
    if (length(label)==0 | length(description)==0 | length(iri)==0 | length(ontology)==0) {
        
            # do rols query
            db=rols::Ontology(ontology)
            tm=rols::term(db,id)
            
            label=rols::termLabel(tm)
            description=rols::termDesc(tm)
            iri=tm@iri
        
    }
    
    # new object
    out = .ontology_term(
        id=id,
        label=label,
        description=description,
        ontology=ontology,
        iri=iri)
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
#' OT = ontology_term(ontology='stato',id='STATO:0000555')
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
        
        # padding for long descriptions and names
        pad = '\n               '
        desc=paste0(strwrap(object$description,width=95,exdent = 2),collapse=pad)
        label=paste0(strwrap(object$label,width=95,exdent = 2),collapse=pad)
        cat('term id:       ',object$id,'\n',sep='')
        cat('ontology:      ',object$ontology,'\n',sep='')
        cat('label:         ',label,'\n',sep='')
        cat('description:   ',desc,'\n',sep='')
        cat('iri:           ',object$iri,'\n',sep='')
        
    }
)


setAs("ontology_term", "data.frame",
    function(from)
        data.frame(
            id=from@id, 
            label=from@label, 
            description=from@description,
            ontology=from@ontology,
            iri=from@iri
        )
)





#' ontology_list object
#'
#' A base class in the \pkg{struct} package. Stores
#' multiple `ontology_term` objects.
#'
#' @export ontology_list
#' @param terms A list of ontology_term objects.
#' @include generics.R
#' @examples
#' \dontrun{
#' OT = ontology_list(terms=list(
#'     ontology_term(ontology='obi',id =  'OBI:0200051'),
#'     ontology_term(ontology='stato',id ='STATO:0000555')
#' )
#' }
#' @rdname ontology
ontology_list = function(terms=list()) {
    
    if (is.null(terms) | length(terms)==0) {
        # empty list
        terms=list()
    }
    
    if (is(terms,'character')){
        # try and convert to terms
        terms=lapply(terms,function(x){
            ontology_term(id=x)
        })
    }
    
    # if ontology_item is provided convert to a list
    if (is(terms,'ontology_term')){
        terms=list(terms)
    }
    
    # check all terms are ontology_item
    if (length(terms)>0) {
        ok=lapply(terms,is,class='ontology_term')
        if (!(all(unlist(ok)))){
            stop('all ontologies must be in "ontology_term" format')
        }
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
#' OL = ontology_list('STATO:0000555')
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
#' OL = ontology_list('STATO:0000555')
#' OL[1]
#' }
#'
#' @return model at the given index in the sequence
setMethod(f = "[",
    signature = "ontology_list",
    definition = function(x,i) {
        return(x@terms[[i]])
    }
)

#' @rdname ontology
#' @param x the list
#' @param i The list item index
#' @param value an ontology_term() object
#' @export
#' @examples
#' \dontrun{
#' OL = ontology_list('STATO:0000555')
#' OL[1] = ontology_term('STATO:0000302')
#' }
#' @return model sequence with the model at index i replaced
setMethod(f = "[<-",
    signature = "ontology_list",
    definition = function(x,i,value) {
        if (!is(value,'ontology_term')) {
            stop('value must be an ontology_term')
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
#' @return the number of models in the sequence
setMethod(f = 'length',
    signature = 'ontology_list',
    definition = function(x) {
        return(length(x@terms))
    }
)

setAs("ontology_list", "data.frame",
    function(from) {
        out=lapply(from@terms,as,Class='data.frame')
        out=do.call(rbind,out)
    }
)

#' catenate ontology_lists
#' 
#' ontology_lists can be catenated with other ontology lists or with ontology_items.
#' @param x an ontology_list()
#' @param ... any number of ontology_list() or ontology_item() objects to catenate
#' @return an ontology_list()
setMethod(f = "c",
    signature = "ontology_list",
    definition = function(x,...) {
        y=list(...)
        check=all(unlist(lapply(y,is,class2='ontology_list')))
        if (!check) {stop('can only combine ontology_list objects')}
        
        x=x@terms
        y=lapply(y,function(x){
            return(x@terms)
        })
        
        terms=c(x,unlist(y))
        out=ontology_list(terms)
        
        return(out)
    }
)