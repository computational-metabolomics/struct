#' Constructor for struct chart objects
#' 
#' A base class in the \pkg{struct} package. Should not be called directly.
#'
#' The \code{chart} class provides a template for figures, charts and plots
#' associated with other objects. For example, a DatasetExperiment object could have a
#' histogram plotted for a specified column.
#'
#' Charts can have parameters but not outputs (other than the figure itself), as
#' chart objects are not intended to be used for calculations. The
#' \code{chart_plot} method can be used to display a chart for an object, and
#' \code{chart_names} can be used to list all chart objects associated with an
#' object.
#'
#' Classes that inherit the \code{stato} class have STATO integration enabled, 
#' allowing stato_id to be set and formal names
#' and descriptions pulled from the STATO ontology database.
#'
#' @export chart
#' @include generics.R struct_class.R parameter_class.R
#' @return a chart object
#' @examples
#' C = example_chart()
#' @return a struct_class object
#' @export
#' @param ... named slots and their values that get passed to struct_class
chart = function(...) {
    # new object
    out = new_struct('chart',...)
    return(out)
}

.chart<-setClass(
    "chart",
    contains = c('struct_class')
)

#' @describeIn chart_plot 
#' @export
setMethod(f = "chart_plot",
    signature = "chart",
    definition = function(obj,dobj) {
        warning('no chart defined for "',class(dobj),'"')
        return(obj)
    }
)

#' Get preset from chart object
#'
#' @param obj A chart object
#' @param preset_name Character string naming the preset to retrieve
#' @param slot_name Character string naming the slot containing a layer_entity
#' @return The preset configuration
#' @export
#' @examples
#' # Get a preset from a chart object
#' C = new_scatter_chart()
#' get_preset(C, 'default', 'points')
#' @rdname get_preset
setMethod(f = "get_preset",
    signature = c('chart','character','character'),
    definition = function(obj, preset_name, slot_name) {
        
        # Check if the slot exists
        if (!slot_name %in% slotNames(obj)) {
            stop('Slot "', slot_name, '" does not exist in chart object of class "', class(obj), '"')
        }
        
        # Get the slot value
        slot_value = slot(obj, slot_name)
        
        # Check if it's a layer_entity
        if (!is(slot_value, 'layer_entity')) {
            stop('Slot "', slot_name, '" is not a layer_entity. It is a "', class(slot_value), '"')
        }
        
        # Get the preset from the layer_entity
        return(get_preset(slot_value, preset_name))
    }
)

setMethod(f = "show",
    signature = c("chart"),
    definition = function(object) {
        callNextMethod()
    }
)

# autocompletion
#' @export
#' @rdname autocompletion
#' @method .DollarNames chart
.DollarNames.chart <- function(x, pattern = "") {
    .DollarNames.struct_class(x,pattern)
}

#' @export 
#' @rdname autocompletion
setMethod('.DollarNames','chart',.DollarNames.chart)

