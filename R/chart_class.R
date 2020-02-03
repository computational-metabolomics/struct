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
    out = .chart(...)
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
        warning(paste0('no chart defined for "',class(dobj),'"'))
        return(obj)
    }
)


setMethod(f = "show",
    signature = c("chart"),
    definition = function(object) {
        callNextMethod()
    }
)
