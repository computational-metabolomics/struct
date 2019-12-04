#' chart_class
#'
#' A base class in the \pkg{struct} package. Should not be called directly.
#'
#' The \code{chart} class provides a template for figures, charts and plots
#' associated with other objects. For example, a dataset object could have a
#' histogram plotted for a specified column.
#'
#' Charts can have parameters but not outputs (other than the figure itself), as
#' chart objects are not intended to be used for calculations. The
#' \code{chart.plot} method can be used to display a chart for an object, and
#' \code{chart.names} can be used to list all chart objects associated with an
#' object.
#'
#' Classes that inherit the \code{chart.stato} class instead of \code{chart}
#' have STATO integration enabled, allowing stato.id to be set and formal names
#' and descriptions pulled from the STATO ontology database.
#'
#' @export chart
#' @include generics.R struct_class.R parameter_class.R
#' @rdname charts
#' @return a chart object
#' @examples
#' # define a new chart object class
#' example_chart = setClass('example_chart',
#'     contains = 'chart',                  # inherit the chart template
#'     slots = c('params.column' = 'numeric') # add a parameter
#' )
#'
#' # define the chart.plot method for the example_chart
#' setMethod('chart.plot',                     # name of the method
#'     signature('example_chart','dataset'),   # the class for each input
#'     definition = function(obj, dobj) {  # function definition (from template)
#'         p = hist(dobj$data[,obj$column])      # the plot
#'         return(p)
#'     }
#' )
#'
#' # create the chart object
#' C = example_chart(column = 2) # set the column parameter to 2
#'
#' # plot
#' p = chart.plot(C,iris_dataset()) # plots a histogram of the second column
#'
chart<-setClass(
    "chart",
    contains = c('struct_class','parameter_class')
)

#' @param obj a chart object
#' @rdname charts
#' @export
setMethod(f = "chart.plot",
    signature = "chart",
    definition = function(obj) {
        warning('no chart defined')
        return(obj)
    }
)


