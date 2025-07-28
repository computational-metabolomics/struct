#' prediction_node class
#'
#' A class for representing prediction nodes in a directed acyclic graph (DAG) of struct models.
#' Each prediction node receives a trained model via an edge and executes model_predict.
#'
#' @export prediction_node
#' @param name the name of the node
#' @param description a description of the node
#' @param ... additional parameters to pass to struct_class
#' @include generics.R struct_class.R struct_node_class.R model_class.R
#' @examples
#' NODE = prediction_node(name = 'Example Prediction', description = 'A simple example prediction node')
#' @rdname prediction_node
prediction_node = function(name = character(0), description = character(0), ...) {
    # new object
    out = new_struct('prediction_node',
        name = name,
        description = description,
        ...)
    return(out)
}

.prediction_node <- setClass(
    "prediction_node",
    contains = c('struct_node'),
    slots = c(
        input_data = 'ANY',
        input_model = 'ANY'
    ),
    prototype = list(
        input_data = NULL,
        input_model = NULL
    )
)



setMethod(f = "show",
    signature = c("prediction_node"),
    definition = function(object) {
        callNextMethod()
        cat('type:          prediction node (receives model via edge)\n', sep = '')
        cat('\n')
    }
)

# autocompletion
#' @export
#' @rdname autocompletion
#' @method .DollarNames prediction_node
.DollarNames.prediction_node <- function(x, pattern = "") {
    .DollarNames.struct_class(x, pattern)
}

#' @export
#' @rdname autocompletion
setMethod('.DollarNames','prediction_node',.DollarNames.prediction_node)
