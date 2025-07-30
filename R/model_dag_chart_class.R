#' @eval get_description('model_dag_chart')
#' @import struct
#' @import ggplot2
#' @importFrom igraph graph_from_data_frame layout_with_sugiyama
#' @export model_dag_chart
#' @examples
#' # Create a simple DAG with nodes
#' D = iris_DatasetExperiment()
#' dag = model_dag(
#'     name = 'Example DAG',
#'     description = 'A simple example DAG',
#'     edges = list(
#'         list(from = 'Raw Data', from_param = 'asis', to = 'PCA', to_param = 'input_data')
#'     ),
#'     nodes = list(
#'         'Raw Data' = data_node(name = 'Raw Data', data = D),
#'         'PCA' = model_node(name = 'PCA', model = PCA())
#'     )
#' )
#' # Execute the DAG to get results
#' dag = dag_execute(dag)
#' C = model_dag_chart()
#' chart_plot(C, dag)
#'
model_dag_chart = function(
        node_size = 5,
        node_color = '#4A90E2',
        edge_color = '#666666',
        edge_width = 1,
        text_size = 4,
        layout_type = 'sugiyama',
        show_labels = TRUE,
        show_parameters = TRUE,
        parameter_text_size = 3,
        box_width = 0.5,
        layout_scale = 2.0,
        ...) {
    out = struct::new_struct('model_dag_chart',
                             node_size = node_size,
                             node_color = node_color,
                             edge_color = edge_color,
                             edge_width = edge_width,
                             text_size = text_size,
                             layout_type = layout_type,
                             show_labels = show_labels,
                             show_parameters = show_parameters,
                                                          parameter_text_size = parameter_text_size,
                             box_width = box_width,
                             layout_scale = layout_scale,
                             ...)
    return(out)
}

.model_dag_chart <- setClass(
    "model_dag_chart",
    contains = 'chart',
    slots = c(
        # INPUTS
        node_size = 'entity',
        node_color = 'entity',
        edge_color = 'entity',
        edge_width = 'entity',
        text_size = 'entity',
        layout_type = 'enum',
        show_labels = 'entity',
        show_parameters = 'entity',
        parameter_text_size = 'entity',
        box_width = 'entity',
        layout_scale = 'entity'
    ),

    prototype = list(
        name = 'Model DAG Chart',
        description = 'Plots a directed acyclic graph (DAG) structure of model objects.',
        type = "dag",
        .params = c('node_size', 'node_color', 'edge_color', 'edge_width',
                    'text_size', 'layout_type', 'show_labels', 'show_parameters',
                    'parameter_text_size', 'box_width', 'layout_scale'),

        node_size = entity(
            name = 'Node size',
            value = 5,
            type = 'numeric',
            description = 'Size of the nodes in the DAG plot',
            max_length = 1
        ),

        node_color = entity(
            name = 'Node color',
            value = '#4A90E2',
            type = 'character',
            description = 'Color of the nodes in the DAG plot',
            max_length = 1
        ),

        edge_color = entity(
            name = 'Edge color',
            value = '#666666',
            type = 'character',
            description = 'Color of the edges in the DAG plot',
            max_length = 1
        ),

        edge_width = entity(
            name = 'Edge width',
            value = 1,
            type = 'numeric',
            description = 'Width of the edges in the DAG plot',
            max_length = 1
        ),

        text_size = entity(
            name = 'Text size',
            value = 4,
            type = 'numeric',
            description = 'Size of the text labels in the DAG plot',
            max_length = 1
        ),

        layout_type = enum(
            name = 'Layout type',
            value = 'sugiyama',
            type = 'character',
            description = c(
                'sugiyama' = 'Hierarchical layout optimized for DAGs',
                'tree' = 'Tree layout',
                'circle' = 'Circular layout',
                'random' = 'Random layout'
            ),
            allowed = c('sugiyama', 'tree', 'circle', 'random')
        ),

        show_labels = entity(
            name = 'Show labels',
            value = TRUE,
            type = 'logical',
            description = 'Whether to show node labels on the plot',
            max_length = 1
        ),

        show_parameters = entity(
            name = 'Show parameters',
            value = TRUE,
            type = 'logical',
            description = 'Whether to show model parameters in node boxes',
            max_length = 1
        ),

        parameter_text_size = entity(
            name = 'Parameter text size',
            value = 3,
            type = 'numeric',
            description = 'Size of the parameter text in node boxes',
            max_length = 1
        ),

        box_width = entity(
            name = 'Box width',
            value = 0.5,
            type = 'numeric',
            description = 'Width of the node boxes',
            max_length = 1
        ),
        
        layout_scale = entity(
            name = 'Layout scale',
            value = 2.0,
            type = 'numeric',
            description = 'Scale factor for node spacing in the layout',
            max_length = 1
        )


    )
)

#' @importFrom igraph graph_from_data_frame layout_with_sugiyama layout_as_tree layout_in_circle layout_randomly
#' @export
setMethod(f = "chart_plot",
          signature = c("model_dag_chart", "model_dag"),
          definition = function(obj, dobj) {

              # Extract edges from the DAG
              edges_list = dobj$edges

              if (length(edges_list) == 0) {
                  # Create a simple plot for empty DAG
                  p = ggplot() +
                      annotate("text", x = 0.5, y = 0.5,
                               label = "Empty DAG\nNo edges defined",
                               size = obj$text_size) +
                      xlim(0, 1) + ylim(0, 1) +
                      theme_void() +
                      ggtitle(dobj$name)
                  return(p)
              }

              # Convert edges list to data frame
              edges_df = do.call(rbind, lapply(edges_list, function(edge) {
                  data.frame(from = edge$from, to = edge$to, stringsAsFactors = FALSE)
              }))

              # Create igraph object
              g = igraph::graph_from_data_frame(edges_df, directed = TRUE)

              # Get layout based on type
              layout_func = switch(obj$layout_type,
                                   'sugiyama' = igraph::layout_with_sugiyama,
                                   'tree' = igraph::layout_as_tree,
                                   'circle' = igraph::layout_in_circle,
                                   'random' = igraph::layout_randomly)

              if (obj$layout_type == 'sugiyama') {
                  layout_coords = layout_func(g)$layout
              } else {
                  layout_coords = layout_func(g)
              }
              
              # Scale layout to provide more space between nodes
              layout_coords = layout_coords * obj$layout_scale

              # Create node data frame
              nodes_df = data.frame(
                  name = igraph::V(g)$name,
                  x = layout_coords[, 1],
                  y = layout_coords[, 2],
                  stringsAsFactors = FALSE
              )

              # Add node type and color information
              nodes_df$node_type = sapply(nodes_df$name, function(node_name) {
                  if (node_name %in% names(dobj$nodes)) {
                      node = dobj$nodes[[node_name]]
                      if (is(node, 'data_node')) {
                          'data'
                      } else if (is(node, 'model_node')) {
                          'model'
                      } else if (is(node, 'prediction_node')) {
                          'prediction'
                      } else if (is(node, 'chart_node')) {
                          'chart'
                      } else {
                          'unknown'
                      }
                  } else {
                      'unknown'
                  }
              })

              # Define colors for different node types
              node_colors = c(
                  'data' = '#4A90E2',      # Blue for data nodes
                  'model' = '#7ED321',     # Green for model nodes
                  'prediction' = '#F5A623', # Orange for prediction nodes
                  'chart' = '#9B59B6',     # Purple for chart nodes
                  'unknown' = '#D0021B'     # Red for unknown nodes
              )

              nodes_df$color = node_colors[nodes_df$node_type]

              # Add parameter information if requested
              if (obj$show_parameters) {
                  # Calculate parameters and line counts for each node
                  node_info = sapply(nodes_df$name, function(node_name) {
                      if (node_name %in% names(dobj$nodes)) {
                          node = dobj$nodes[[node_name]]
                          if (is(node, 'data_node')) {
                              # For data nodes, just show the node name
                              list(parameters = node_name, lines = 1)
                          } else if (is(node, 'model_node')) {
                              # For model nodes, extract parameters from the model
                              model_obj = model(node)
                              param_names = param_ids(model_obj)
                                                             if (length(param_names) > 0) {
                                   param_values = sapply(param_names, param_value, obj = model_obj)
                                   # Create parameter string with title
                                   param_strings = paste(param_names, param_values, sep = " = ")
                                   # Limit to first 3 parameters to prevent box overflow
                                   if (length(param_strings) > 3) {
                                       param_strings = c(param_strings[1:3], "...")
                                   }
                                   full_text = paste0(param_strings, collapse='\n')
                                   list(parameters = full_text, lines = min(length(param_strings) + 1, 4))
                               } else {
                                   list(parameters = paste(node_name, "no parameters", sep = "\n"), lines = 2)
                               }
                          } else if (is(node, 'prediction_node')) {
                              # For prediction nodes, show the node name
                              list(parameters = node_name, lines = 1)
                          } else if (is(node, 'chart_node')) {
                              # For chart nodes, extract parameters from the chart
                              chart_obj = chart(node)
                              param_names = param_ids(chart_obj)
                                                             if (length(param_names) > 0) {
                                   param_values = sapply(param_names, param_value, obj = chart_obj)
                                   # Create parameter string with title
                                   param_strings = paste(param_names, param_values, sep = " = ")
                                   # Limit to first 3 parameters to prevent box overflow
                                   if (length(param_strings) > 3) {
                                       param_strings = c(param_strings[1:3], "...")
                                   }
                                   full_text = paste0(param_strings, collapse='\n')
                                   list(parameters = full_text, lines = min(length(param_strings) + 1, 4))
                               } else {
                                   list(parameters = paste(node_name, "no parameters", sep = "\n"), lines = 2)
                               }
                          } else {
                              list(parameters = "unknown node type", lines = 1)
                          }
                      } else {
                          list(parameters = "node not found", lines = 1)
                      }
                  }, simplify = FALSE)

                  # Extract parameters and calculate max lines
                  nodes_df$parameters = sapply(node_info, function(x) x$parameters)
                  max_lines = max(sapply(node_info, function(x) x$lines))

                  # Calculate dynamic box height based on max lines with scale factor
                  line_height = 0.3  # Height per line
                  # Limit box height to prevent overlap - max 3 lines
                  max_lines_capped = min(max_lines, 3)
                  dynamic_box_height = max_lines_capped * line_height * 0.5  # Scale factor of 0.5
              }

              # Create edge data frame for plotting with arrows touching box edges
              edge_coords = data.frame()
              for (i in 1:nrow(edges_df)) {
                  from_node = edges_df$from[i]
                  to_node = edges_df$to[i]

                  from_coords = nodes_df[nodes_df$name == from_node, c('x', 'y')]
                  to_coords = nodes_df[nodes_df$name == to_node, c('x', 'y')]

                                        # Calculate direction vector
                      dx = to_coords$x - from_coords$x
                      dy = to_coords$y - from_coords$y
                      length = sqrt(dx^2 + dy^2)

                      if (length > 0) {
                          # Normalize direction vector
                          dx = dx / length
                          dy = dy / length

                          # Use dynamic box height if available, otherwise use default
                          box_height = if (exists('dynamic_box_height')) dynamic_box_height else 0.5

                          # Calculate box dimensions
                          box_width = obj$box_width
                          box_height_actual = box_height

                          # Calculate intersection points with box edges
                          # For horizontal edges (dx > dy)
                          if (abs(dx) > abs(dy)) {
                              # Start point: edge of from box
                              if (dx > 0) {
                                  start_x = from_coords$x + box_width/2
                                  start_y = from_coords$y + dy * box_height_actual/2
                              } else {
                                  start_x = from_coords$x - box_width/2
                                  start_y = from_coords$y + dy * box_height_actual/2
                              }

                              # End point: edge of to box
                              if (dx > 0) {
                                  end_x = to_coords$x - box_width/2
                                  end_y = to_coords$y + dy * box_height_actual/2
                              } else {
                                  end_x = to_coords$x + box_width/2
                                  end_y = to_coords$y + dy * box_height_actual/2
                              }
                          } else {
                              # For vertical edges (dy >= dx)
                              # Start point: edge of from box
                              if (dy > 0) {
                                  start_x = from_coords$x + dx * box_width/2
                                  start_y = from_coords$y + box_height_actual/2
                              } else {
                                  start_x = from_coords$x + dx * box_width/2
                                  start_y = from_coords$y - box_height_actual/2
                              }

                              # End point: edge of to box
                              if (dy > 0) {
                                  end_x = to_coords$x + dx * box_width/2
                                  end_y = to_coords$y - box_height_actual/2
                              } else {
                                  end_x = to_coords$x + dx * box_width/2
                                  end_y = to_coords$y + box_height_actual/2
                              }
                          }
                      } else {
                          # If nodes are at same position, use original coordinates
                          start_x = from_coords$x
                          start_y = from_coords$y
                          end_x = to_coords$x
                          end_y = to_coords$y
                      }

                  edge_coords = rbind(edge_coords,
                                      data.frame(x = start_x, y = start_y,
                                                 xend = end_x, yend = end_y,
                                                 group = i))
              }

              # Create the plot
              p = ggplot() +
                  # Add edges
                  geom_segment(data = edge_coords,
                               aes(x = x, y = y, xend = xend, yend = yend),
                               color = obj$edge_color,
                               size = obj$edge_width,
                               arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
                                                      # Add node boxes
                  geom_rect(data = nodes_df,
                            aes(xmin = x - obj$box_width/2,
                                xmax = x + obj$box_width/2,
                                ymin = y - (if (exists('dynamic_box_height')) dynamic_box_height else 0.5)/2,
                                ymax = y + (if (exists('dynamic_box_height')) dynamic_box_height else 0.5)/2,
                                fill = node_type),
                            color = "black",
                            alpha = 0.8) +

                  # Add bold node titles
                  {if (obj$show_labels) {
                      geom_text(data = nodes_df,
                               aes(x = x, y = y + (if (exists('dynamic_box_height')) dynamic_box_height else 0.5)/2 - 0.05, label = name),
                               size = obj$text_size,
                               hjust = 0.5,
                               vjust = 0.5,
                               fontface = "bold",
                               check_overlap = TRUE)
                  } else {
                      NULL
                  }} +

                  # Add parameter text
                  {if (obj$show_parameters) {
                      geom_text(data = nodes_df,
                               aes(x = x, y = y, label = parameters),
                               size = obj$parameter_text_size,
                               hjust = 0.5,
                               vjust = 0.5,
                               check_overlap = TRUE)
                  } else {
                      NULL
                  }} +
                  # Theme and styling
                  theme_void() +
                  theme(plot.title = element_text(hjust = 0.5, size = 12),
                        plot.subtitle = element_text(hjust = 0.5, size = 10),
                        plot.margin = margin(20, 20, 20, 20)) +
                  guides(fill = "none") +
                  ggtitle(dobj$name, subtitle = dobj$description) +
                  scale_fill_manual(values=node_colors)
              # Equal aspect ratio with proper expansion
              #coord_fixed(ratio = 1, expand = TRUE)

              return(p)
          }
)
