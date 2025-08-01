#' data_node class
#'
#' A class for representing data nodes in a directed acyclic graph (DAG) of struct models.
#' Each data node contains a DatasetExperiment object that can be used as input to other nodes.
#'
#' @section \code{data} slot:
#' The "data" slot holds a DatasetExperiment object that will be used as input
#' to other nodes in the DAG.
#'
#' @export data_node
#' @param name the name of the data node
#' @param description a description of the data node
#' @param data a DatasetExperiment object
#' @param ... additional parameters to pass to struct_class
#' @include generics.R struct_class.R struct_node_class.R DatasetExperiment_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' DNODE = data_node(name = 'Example Data', description = 'A simple example data node', data = D)
#' @rdname data_node
data_node = function(name = character(0), description = character(0), data = NULL, ...) {
    # new object
    out = new_struct('data_node',
        name = name,
        description = description,
        data = data,
        ...)
    return(out)
}

.data_node <- setClass(
    "data_node",
    contains = c('struct_node'),
    slots = c(
        data = 'DatasetExperiment',
        input_data = 'ANY'
    ),
    prototype = list(
        data = NULL,
        .params=c('data','input_data')
    )
)

#' @rdname data_value
#' @export
#' @examples
#' D = iris_DatasetExperiment()
#' DNODE = data_node(data = D)
#' data_value(DNODE)
#' @return the DatasetExperiment object in the data node
setGeneric("data_value", function(object) standardGeneric("data_value"))

#' @rdname data_value
#' @export
setMethod(f = "data_value",
    signature = c("data_node"),
    definition = function(object) {
        return(object@data)
    }
)

#' @rdname data_value
#' @export
#' @examples
#' D = iris_DatasetExperiment()
#' DNODE = data_node()
#' data_value(DNODE) = D
#' @return the modified data node object
setGeneric("data_value<-", function(object, value) standardGeneric("data_value<-"))

#' @rdname data_value
#' @export
setMethod(f = "data_value<-",
    signature = c("data_node", "DatasetExperiment"),
    definition = function(object, value) {
        object@data = value
        return(object)
    }
)

setMethod(f = "show",
    signature = c("data_node"),
    definition = function(object) {
        callNextMethod()
        if (!is.null(object@data)) {
            cat('data:          ', nrow(object@data$data), ' rows x ', ncol(object@data$data), ' columns\n', sep = '')
        } else {
            cat('data:          NULL\n', sep = '')
        }
        cat('\n')
    }
)

# autocompletion
#' @export
#' @rdname autocompletion
#' @method .DollarNames data_node
.DollarNames.data_node <- function(x, pattern = "") {
    .DollarNames.struct_class(x, pattern)
}

#' @export
#' @rdname autocompletion
setMethod('.DollarNames','data_node',.DollarNames.data_node)
