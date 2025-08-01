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
#' @include generics.R struct_class.R entity_class.R output_class.R
#' @examples
#' DAG = model_dag(name = 'Example DAG', description = 'A simple example DAG')
#' @rdname model_dag
model_dag = function(name = character(0), description = character(0), edges = list(), nodes = list(), results = list(), ...) {
    # new object
    out = new_struct('model_dag',
        name = name,
        description = description,
        edges = edges,
        nodes = nodes,
        results = results,
        ...)
    return(out)
}

.model_dag <- setClass(
    "model_dag",
    contains = c('struct_class'),
    slots = c(
        edges = 'entity',
        nodes = 'entity',
        results = 'entity'
    ),
    prototype = list(
        name = 'Model DAG',
        description = 'A directed acyclic graph of struct models',
        type = 'dag',
        .params = c('edges', 'nodes', 'results'),
        
        edges = entity(
            name = 'Edges',
            value = list(),
            type = 'list',
            description = 'List of edges defining the DAG structure',
            max_length = Inf
        ),
        
        nodes = entity(
            name = 'Nodes',
            value = list(),
            type = 'list',
            description = 'List of nodes (data_node, model_node, prediction_node objects)',
            max_length = Inf
        ),
        
        results = entity(
            name = 'Results',
            value = list(),
            type = 'list',
            description = 'Results of DAG execution',
            max_length = Inf
        )
    )
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