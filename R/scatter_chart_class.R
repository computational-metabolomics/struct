#' New Scatter Chart objects
#'
#' A chart class in the \pkg{struct} package for creating scatter plots using the new ggplot_chart approach.
#' This chart extends ggplot_chart to provide scatter plot functionality with configurable layers.
#'
#' @export
#' @import ggplot2
#' @param xcol Character string naming the x-axis column
#' @param ycol Character string naming the y-axis column
#' @param factor_name Character string naming the factor column for grouping
#' @param points Layer entity for points layer
#' @param ellipse Layer entity for ellipse layer
#' @param point_labels Layer entity for point labels layer
#' @param ... Additional parameters passed to ggplot_chart constructor
#' @return A new_scatter_chart object
#' @examples
#' # Create a scatter chart
#' D = iris_DatasetExperiment()
#' C = new_scatter_chart(
#'     xcol = 'Petal.Width',
#'     ycol = 'Sepal.Width',
#'     factor_name = 'Species'
#' )
#' chart_plot(C, D)
#' @rdname new_scatter_chart
#' @include ggplot_chart_class.R layer_entity_class.R struct_preset_class.R typed_list_class.R
new_scatter_chart = function(
    xcol = 'Petal.Width',
    ycol = 'Sepal.Width',
    factor_name = 'Species',
    points = layer_entity(
        layer = 'geom_point',
        value = preset('default'),
        presets = list(
            none = NULL,
            default = list(
                mapping = aes(
                    colour = .data[['colour']],
                    shape = .data[['shape']]
                )
            )
        )
    ),
    ellipse = layer_entity(
        layer = 'stat_ellipse',
        value = preset('none'),
        presets = list(
            none = NULL,
            sample_ellipse = list(
                mapping = aes(
                    x = .data[['x']],
                    y = .data[['y']]
                ),
                colour = "grey",
                type = 'norm'
            ),
            group_ellipse = list(
                mapping = aes(
                    x = .data[['x']],
                    y = .data[['y']],
                    color = .data[['colour']]
                ),
                type = 'norm'
            )
        )
    ),
    point_labels = layer_entity(
        layer = 'geom_text',
        value = preset('none'),
        presets = list(
            none = NULL,
            all_points = list(
                mapping = aes(
                    x = .data[['x']],
                    y = .data[['y']],
                    color = .data[['colour']],
                    label = .data[['label']]
                ),
                hjust = 'outward',
                vjust = 'outward'
            )
        )
    ),
    ...) {
    
    out = new_struct('new_scatter_chart',
        xcol = xcol,
        ycol = ycol,
        factor_name = factor_name,
        points = points,
        ellipse = ellipse,
        point_labels = point_labels,
        ...)
    return(out)
}

.new_scatter_chart<-setClass(
    "new_scatter_chart",
    contains = 'ggplot_chart',
    slots = c(
        # INPUTS
        xcol = 'entity',
        ycol = 'entity',
        factor_name = 'entity',
        points = 'layer_entity',
        ellipse = 'layer_entity',
        point_labels = 'layer_entity'
    ),
    
    prototype = list(
        name = 'New Group scatter chart',
        description = 'Plots a 2d scatter plot of the input data using the new ggplot_chart approach.',
        type = "scatter",
        .params = c('xcol', 'ycol', 'factor_name', 'points', 'ellipse', 'point_labels'),
        
        xcol = entity(
            name = 'x-axis column name',
            value = 'Petal.Width',
            type = c('numeric', 'integer', 'character'),
            description = 'The column name, or index, of data to plot on the x-axis',
            max_length = 1
        ),
        
        ycol = entity(
            name = 'y-axis column name',
            value = 'Sepal.Width',
            type = c('numeric', 'integer', 'character'),
            description = 'The column name, or index, of data to plot on the y-axis',
            max_length = 1
        ),
        
        factor_name = entity(
            name = 'Factor name',
            description = 'The name of a sample-meta column to use for grouping.',
            type = 'character',
            value = 'Species'
        ),
        
        points = layer_entity(
            layer = 'geom_point',
            value = preset('default'),
            presets = list(
                none = NULL,
                default = list(
                    mapping = aes(
                        colour = .data[['colour']],
                        shape = .data[['shape']]
                    )
                )
            )
        ),
        
        ellipse = layer_entity(
            layer = 'stat_ellipse',
            value = preset('none'),
            presets = list(
                none = NULL,
                sample_ellipse = list(
                    mapping = aes(
                        x = .data[['x']],
                        y = .data[['y']]
                    ),
                    colour = "grey",
                    type = 'norm'
                ),
                group_ellipse = list(
                    mapping = aes(
                        x = .data[['x']],
                        y = .data[['y']],
                        color = .data[['colour']]
                    ),
                    type = 'norm'
                )
            )
        ),
        
        point_labels = layer_entity(
            layer = 'geom_text',
            value = preset('none'),
            presets = list(
                none = NULL,
                all_points = list(
                    mapping = aes(
                        x = .data[['x']],
                        y = .data[['y']],
                        color = .data[['colour']],
                        label = .data[['label']]
                    ),
                    hjust = 'outward',
                    vjust = 'outward'
                )
            )
        ),
        
        mapping = entity(
            name = 'Plot aesthetics',
            description = 'A list of plot aesthetics and their mapping to the data',
            value = aes(
                x = .data[['x']],
                y = .data[['y']]
            ),
            type = c('typed_list.uneval', 'uneval', 'NULL')
        )
    )
)

#' @rdname new_scatter_chart
#' @export
setMethod(f = "chart_build",
    signature = c("new_scatter_chart", 'DatasetExperiment'),
    definition = function(obj, dobj) {
        
        # Convert column indices to names if needed
        if (is.numeric(obj@xcol@value)) {
            obj@xcol@value = colnames(dobj)[obj@xcol@value]
        }
        if (is.numeric(obj@ycol@value)) {
            obj@ycol@value = colnames(dobj)[obj@ycol@value]
        }
        
        # Get the data columns
        x_data = dobj$data[, obj@xcol@value, drop = FALSE]
        y_data = dobj$data[, obj@ycol@value, drop = FALSE]
        
        # Get the factor data
        factor_data = dobj$sample_meta[, obj@factor_name@value, drop = FALSE]
        
        # Create the combined data frame with proper column names
        plot_data = data.frame(
            x = x_data[[1]],
            y = y_data[[1]],
            colour = factor_data[[1]],
            shape = factor_data[[1]],  # Use same factor for shape initially
            label = rownames(dobj$sample_meta)  # Use row names as labels
        )
        
        # Store the prepared data
        obj@data = plot_data
        
        
        # Build all layers and add them to the unified layers slot
        layers_list = list()
        
        # Add points layer
        points_layer = as_layer(obj@points)
        if (!is.null(points_layer)) {
            layers_list$points = points_layer
        }
        
        # Add ellipse layer
        ellipse_layer = as_layer(obj@ellipse)
        if (!is.null(ellipse_layer)) {
            layers_list$ellipse = ellipse_layer
        }
        
        # Add point labels layer
        point_labels_layer = as_layer(obj@point_labels)
        if (!is.null(point_labels_layer)) {
            layers_list$point_labels = point_labels_layer
        }
        
        # Add axis labels
        layers_list$xlab = xlab(obj@xcol@value)
        layers_list$ylab = ylab(obj@ycol@value)
        
        # Add default theme
        layers_list$theme = theme_minimal()
        
        # Create the unified layers typed_list
        obj@layers = typed_list(
            .list = layers_list,
            .type = 'Layer',
            drop_null = TRUE
        )
        
        return(obj)
    }
)

#' @rdname new_scatter_chart
#' @export
setMethod(f = 'show',
    signature = c('new_scatter_chart'),
    definition = function(object) {
        callNextMethod()
        cat('x column:       ', object@xcol@value, '\n', sep = '')
        cat('y column:       ', object@ycol@value, '\n', sep = '')
        cat('factor:         ', object@factor_name@value, '\n', sep = '')
    }
) 