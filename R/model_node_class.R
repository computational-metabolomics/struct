#' model_node class
#'
#' A class for representing nodes in a directed acyclic graph (DAG) of struct models.
#' Each node contains a struct model object that will be executed using model_apply.
#'
#' @section \code{model} slot:
#' The "model" slot holds a struct model object (e.g., PCA, PLS, etc.) that will be
#' executed when the node is processed using model_apply.
#'
#' @export model_node
#' @param name the name of the node
#' @param description a description of the node
#' @param model a struct model object
#' @param ... additional parameters to pass to struct_class
#' @include generics.R struct_class.R struct_node_class.R model_class.R
#' @examples
#' M = example_model()
#' NODE = model_node(name = 'Example Node', description = 'A simple example node', model = M)
#' @rdname model_node
model_node = function(name = character(0), description = character(0), model = NULL, ...) {
    # new object
    out = new_struct('model_node',
        name = name,
        description = description,
        model = model,
        ...)
    return(out)
}

.model_node <- setClass(
    "model_node",
    contains = c('struct_node'),
    slots = c(
        model = 'struct_class',
        input_data = 'ANY'
    ),
    prototype = list(
        model = NULL,
        input_data = NULL,
        .params=c('model','input_data')
    )
)

#' @rdname model
#' @export
#' @examples
#' M = example_model()
#' NODE = model_node(model = M, mode = model_apply)
#' model(NODE)
#' @return the model object in the node
setGeneric("model", function(object) standardGeneric("model"))

#' @rdname model
#' @export
setMethod(f = "model",
    signature = c("model_node"),
    definition = function(object) {
        return(object@model)
    }
)

#' @rdname model
#' @export
#' @examples
#' M = example_model()
#' NODE = model_node()
#' model(NODE) = M
#' @return the modified node object
setGeneric("model<-", function(object, value) standardGeneric("model<-"))

#' @rdname model
#' @export
setMethod(f = "model<-",
    signature = c("model_node", "struct_class"),
    definition = function(object, value) {
        object@model = value
        return(object)
    }
)



setMethod(f = "show",
    signature = c("model_node"),
    definition = function(object) {
        callNextMethod()
        if (!is.null(object@model)) {
            cat('model:         ', class(object@model)[1], '\n', sep = '')
        } else {
            cat('model:         NULL\n', sep = '')
        }
        cat('\n')
    }
)

# autocompletion
#' @export
#' @rdname autocompletion
#' @method .DollarNames model_node
.DollarNames.model_node <- function(x, pattern = "") {
    .DollarNames.struct_class(x, pattern)
}

#' @export
#' @rdname autocompletion
setMethod('.DollarNames','model_node',.DollarNames.model_node)
