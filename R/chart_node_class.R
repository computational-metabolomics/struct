#' chart_node class
#'
#' A class for representing chart nodes in a directed acyclic graph (DAG) of struct models.
#' Each node contains a struct chart object that will be executed using chart_plot.
#' Chart nodes can have multiple input objects (e.g., a model and a dataset).
#'
#' @section \code{chart} slot:
#' The "chart" slot holds a struct chart object that will be executed when the node is processed using chart_plot.
#'
#' @section \code{input_objects} slot:
#' The "input_objects" slot holds a list of input objects that will be passed to chart_plot.
#' The order of objects in this list corresponds to the order of parameters in the chart_plot method.
#'
#' @export chart_node
#' @param name the name of the node
#' @param description a description of the node
#' @param chart a struct chart object
#' @param ... additional parameters to pass to struct_class
#' @include generics.R struct_class.R struct_node_class.R chart_class.R
#' @examples
#' C = example_chart()
#' NODE = chart_node(name = 'Example Chart Node', description = 'A simple example chart node', chart = C)
#' @rdname chart_node
chart_node = function(name = character(0), description = character(0), chart = NULL, ...) {
    # new object
    out = new_struct('chart_node',
        name = name,
        description = description,
        chart = chart,
        ...)
    return(out)
}

.chart_node <- setClass(
    "chart_node",
    contains = c('struct_node'),
    slots = c(
        chart = 'struct_class',
        input_objects = 'list'
    ),
    prototype = list(
        chart = NULL,
        input_objects = list()
    )
)

#' @rdname chart
#' @export
#' @examples
#' C = example_chart()
#' NODE = chart_node(chart = C)
#' chart(NODE)
#' @return the chart object in the node
setGeneric("chart", function(object) standardGeneric("chart"))

#' @rdname chart
#' @export
setMethod(f = "chart",
    signature = c("chart_node"),
    definition = function(object) {
        return(object@chart)
    }
)

#' @rdname chart
#' @export
#' @examples
#' C = example_chart()
#' NODE = chart_node()
#' chart(NODE) = C
#' @return the modified node object
setGeneric("chart<-", function(object, value) standardGeneric("chart<-"))

#' @rdname chart
#' @export
setMethod(f = "chart<-",
    signature = c("chart_node", "struct_class"),
    definition = function(object, value) {
        object@chart = value
        return(object)
    }
)

setMethod(f = "show",
    signature = c("chart_node"),
    definition = function(object) {
        callNextMethod()
        if (!is.null(object@chart)) {
            cat('chart:         ', class(object@chart)[1], '\n', sep = '')
        } else {
            cat('chart:         NULL\n', sep = '')
        }
        if (length(object@input_objects) > 0) {
            cat('input objects: ', length(object@input_objects), ' objects\n', sep = '')
        } else {
            cat('input objects: none\n', sep = '')
        }
        cat('\n')
    }
)

# autocompletion
#' @export
#' @rdname autocompletion
#' @method .DollarNames chart_node
.DollarNames.chart_node <- function(x, pattern = "") {
    .DollarNames.struct_class(x, pattern)
}

#' @export
#' @rdname autocompletion
setMethod('.DollarNames','chart_node',.DollarNames.chart_node) 