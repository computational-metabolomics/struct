#' struct_node class
#'
#' A base class for representing nodes in a directed acyclic graph (DAG) of struct models.
#' All node types (data_node, model_node, prediction_node) inherit from this class.
#'
#' @export struct_node
#' @param name the name of the node
#' @param description a description of the node
#' @param ... additional parameters to pass to struct_class
#' @include generics.R struct_class.R
#' @examples
#' NODE = struct_node(name = 'Example Node', description = 'A simple example node')
#' @rdname struct_node
struct_node = function(name = character(0), description = character(0), ...) {
    # new object
    out = .struct_node(
        name = name,
        description = description,
        ...)
    return(out)
}

.struct_node <- setClass(
    "struct_node",
    contains = c('struct_class'),
    prototype = list()
)

setMethod(f = "show",
    signature = c("struct_node"),
    definition = function(object) {
        callNextMethod()
        cat('type:          struct node\n', sep = '')
        cat('\n')
    }
)

# autocompletion
#' @export
#' @rdname autocompletion
#' @method .DollarNames struct_node
.DollarNames.struct_node <- function(x, pattern = "") {
    .DollarNames.struct_class(x, pattern)
}

#' @export
#' @rdname autocompletion
setMethod('.DollarNames','struct_node',.DollarNames.struct_node) 