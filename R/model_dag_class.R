#' model_dag class
#'
#' A class for representing directed acyclic graphs (DAGs) of struct models.
#' This class extends struct_class and provides functionality for executing
#' workflows defined as directed acyclic graphs.
#'
#' @section \code{edges} slot:
#' The "edges" slot is a list that defines the connections between nodes in the DAG.
#' Each edge should be a list with 'from' and 'to' elements specifying the source
#' and target nodes.
#'
#' @export model_dag
#' @param name the name of the DAG
#' @param description a description of the DAG
#' @param edges a list of edges defining the DAG structure
#' @param ... additional parameters to pass to struct_class
#' @include generics.R struct_class.R
#' @examples
#' DAG = model_dag(name = 'Example DAG', description = 'A simple example DAG')
#' @rdname model_dag
model_dag = function(name = character(0), description = character(0), edges = list(), ...) {
    # new object
    out = new_struct('model_dag',
        name = name,
        description = description,
        edges = edges,
        ...)
    return(out)
}

.model_dag <- setClass(
    "model_dag",
    contains = c('struct_class'),
    slots = c(
        edges = 'list'
    ),
    prototype = list(
        edges = list()
    )
)

#' @rdname edges
#' @export
#' @examples
#' DAG = model_dag(name = 'Example DAG', description = 'A simple example DAG')
#' edges(DAG)
#' @return the edges of the DAG
setGeneric("edges", function(object) standardGeneric("edges"))

#' @rdname edges
#' @export
setMethod(f = "edges",
    signature = c("model_dag"),
    definition = function(object) {
        return(object@edges)
    }
)

#' @rdname edges
#' @export
#' @examples
#' DAG = model_dag()
#' edges(DAG) = list(list(from = 'node1', to = 'node2'))
#' @return the modified DAG object
setGeneric("edges<-", function(object, value) standardGeneric("edges<-"))

#' @rdname edges
#' @export
setMethod(f = "edges<-",
    signature = c("model_dag", "list"),
    definition = function(object, value) {
        object@edges = value
        return(object)
    }
)

setMethod(f = "show",
    signature = c("model_dag"),
    definition = function(object) {
        callNextMethod()
        cat('edges:         ', length(object@edges), ' edges\n', sep = '')
        if (length(object@edges) > 0) {
            for (i in seq_along(object@edges)) {
                edge = object@edges[[i]]
                cat('               ', edge$from, ' -> ', edge$to, '\n', sep = '')
            }
        }
        cat('\n')
    }
)

# autocompletion
#' @export
#' @rdname autocompletion
#' @method .DollarNames model_dag
.DollarNames.model_dag <- function(x, pattern = "") {
    .DollarNames.struct_class(x, pattern)
}

#' @export
#' @rdname autocompletion
setMethod('.DollarNames','model_dag',.DollarNames.model_dag) 