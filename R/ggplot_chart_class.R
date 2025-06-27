#' ggplot Chart objects
#'
#' A base class in the \pkg{struct} package for ggplot2-based charts.
#' ggplot_chart extends the base chart class to provide common functionality
#' for all ggplot2 chart objects.
#'
#' The layers slot can contain any ggplot2 layer including:
#' - Geoms (geom_point, geom_line, etc.)
#' - Stats (stat_ellipse, stat_smooth, etc.)
#' - Scales (scale_color_manual, scale_x_continuous, etc.)
#' - Themes (theme_minimal, theme_bw, etc.)
#' - Facets (facet_wrap, facet_grid, etc.)
#' - Labels (labs, xlab, ylab, etc.)
#'
#' @export
#' @param ... Additional parameters passed to chart constructor
#' @return A ggplot_chart object
#' @examples
#' # Create a ggplot chart (not typically called directly)
#' gc = ggplot_chart()
#' @rdname ggplot_chart
#' @include chart_class.R layer_entity_class.R struct_preset_class.R typed_list_class.R global_preset_registry.R
ggplot_chart = function(...) {
    # new object
    out = new_struct('ggplot_chart', ...)
    return(out)
}

.ggplot_chart<-setClass(
    "ggplot_chart",
    contains = 'chart',
    slots = c(
        data = 'data.frame',
        layers = 'typed_list',
        mapping = 'entity'
    ),
    prototype = list(
        name = 'ggplot chart',
        description = 'A base class for ggplot2-based charts. Supports all ggplot2 layer types including geoms, stats, scales, themes, facets, and labels.',
        type = 'ggplot',
        data = data.frame(),
        layers = typed_list(.type = 'Layer'),
        mapping = entity(
            name = 'Plot aesthetics',
            description = 'A list of plot aesthetics and their mapping to the data',
            value = NULL,
            type = c('typed_list.uneval', 'uneval', 'NULL')
        )
    )
)

#' @rdname ggplot_chart
#' @export
setMethod(f = "chart_build",
    signature = c("ggplot_chart", 'DatasetExperiment'),
    definition = function(obj, dobj) {
        # This is a base method that should be overridden by specific chart types
        warning('chart_build method not implemented for "', class(obj), '"')
        return(obj)
    }
)

#' @rdname ggplot_chart
#' @export
setMethod(f = "chart_plot",
    signature = c("ggplot_chart", 'DatasetExperiment'),
    definition = function(obj, dobj) {
        # Build the chart
        obj = chart_build(obj, dobj)
        
        # Create the ggplot object
        p = ggplot2::ggplot(data = obj@data, mapping = obj@mapping@value)
        
        # Add all layers (geoms, stats, scales, themes, labels, facets, etc.)
        if (length(obj@layers) > 0) {
            for (i in seq_along(obj@layers)) {
                layer = obj@layers[[i]]
                if (!is.null(layer)) {
                    p = p + layer
                }
            }
        }
        
        return(p)
    }
)

#' @rdname ggplot_chart
#' @export
setMethod(f = 'show',
    signature = c('ggplot_chart'),
    definition = function(object) {
        callNextMethod()
        cat('data:          ', nrow(object@data), ' rows x ', ncol(object@data), ' columns\n', sep = '')
        cat('layers:        ', length(object@layers), ' layers\n', sep = '')
    }
)

# Helper functions for common ggplot operations
xlab = function(label) {
    return(ggplot2::xlab(label))
}

ylab = function(label) {
    return(ggplot2::ylab(label))
}
