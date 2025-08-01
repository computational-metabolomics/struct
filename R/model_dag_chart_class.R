#' model_dag_chart class
#'
#' A chart class for visualizing model_dag objects using DiagrammeR.
#' This chart creates a directed acyclic graph visualization showing
#' the nodes and their connections, including node names and model parameters.
#'
#' @export model_dag_chart
#' @param name the name of the chart
#' @param description a description of the chart
#' @param node_width the width of nodes in the graph (default: 2)
#' @param node_height the height of nodes in the graph (default: 1)
#' @param font_size the font size for node labels (default: 8)
#' @param show_params which parameters to show in nodes: "all", "changed", "connected", or "none" (default: "all")
#' @param max_param_length maximum length of parameter text before truncation (default: 50)
#' @param layout the layout algorithm to use (default: "dot")
#' @param rankdir the direction for ranking nodes (default: "LR" for left-to-right)
#' @param node_color the color of nodes (default: "lightblue")
#' @param edge_color the color of edges (default: "darkblue")
#' @param edge_width the width of edges (default: 2)
#' @param arrow_size the size of arrowheads (default: 1.5)
#' @param arrow_shape the shape of arrowheads (default: "normal")
#' @param ... additional parameters to pass to chart
#' @import DiagrammeR
#' @include generics.R struct_class.R chart_class.R
#' @examples
#' C = model_dag_chart()
#' @rdname model_dag_chart
model_dag_chart = function(name = character(0), description = character(0),
                           node_width = 'auto', node_height = 'auto', font_size = 8,
                           show_params = "all",
                           max_param_length = 50, layout = "tree", rankdir = "LR",
                           node_color = "lightblue", edge_color = "darkblue",
                           edge_width = 2, arrow_size = 1.5, arrow_shape = "normal", ...) {
    # new object
    out = new_struct('model_dag_chart',
                     name = name,
                     description = description,
                     node_width = node_width,
                     node_height = node_height,
                     font_size = font_size,
                     show_params = show_params,
                     max_param_length = max_param_length,
                     layout = layout,
                     rankdir = rankdir,
                     node_color = node_color,
                     edge_color = edge_color,
                     edge_width = edge_width,
                     arrow_size = arrow_size,
                     arrow_shape = arrow_shape,
                     ...)
    return(out)
}

.model_dag_chart <- setClass(
    "model_dag_chart",
    contains = c('chart'),
    slots = c(
        node_width = 'entity',
        node_height = 'entity',
        font_size = 'entity',
        show_params = 'entity',
        max_param_length = 'entity',
        layout = 'entity',
        rankdir = 'entity',
        node_color = 'entity',
        edge_color = 'entity',
        edge_width = 'entity',
        arrow_size = 'entity',
        arrow_shape = 'entity'
    ),
    prototype = list(
        name = 'Model DAG Chart',
        description = 'A chart for visualizing model DAGs using DiagrammeR',
        type = 'chart',
        .params = c('node_width', 'node_height', 'font_size',
                    'show_params', 'max_param_length', 'layout', 'rankdir',
                    'node_color', 'edge_color', 'edge_width', 'arrow_size',
                    'arrow_shape'),

        node_width = entity(
            name = 'Node Width',
            value = 'auto',
            type = c('numeric', 'character'),
            description = paste0(
                'Width of nodes in the graph. Use "auto" for auto-sizing or ',
                'a numeric value for fixed width'),
            max_length = 1
        ),

        node_height = entity(
            name = 'Node Height',
            value = 'auto',
            type = c('numeric', 'character'),
            description = paste0(
                'Height of nodes in the graph. Use "auto" for auto-sizing or ',
                'a numeric value for fixed height'),
            max_length = 1
        ),

        font_size = entity(
            name = 'Font Size',
            value = 8,
            type = 'numeric',
            description = 'Font size for node labels',
            max_length = 1
        ),

        show_params = enum(
            name = 'Show Parameters',
            value = 'all',
            description = c(
                'Which parameters to show in nodes:',
                all = 'all parameters',
                changed = "non-default values only",
                connected ="to_param/from_param only",
                none ="no parameters"),
            allowed = c('all', 'changed', 'connected', 'none')
        ),

        max_param_length = entity(
            name = 'Max Parameter Length',
            value = 50,
            type = 'numeric',
            description = 'Maximum length of parameter text before truncation',
            max_length = 1
        ),

        layout = enum(
            name = 'Layout Algorithm',
            value = 'tree',
            type = 'character',
            description = 'The layout algorithm to use for graph positioning',
            max_length = 1,
            allowed = c('circle', 'tree', 'kk', 'fr', 'nicely', 'neato')
        ),

        rankdir = enum(
            name = 'Rank Direction',
            value = 'LR',
            description = 'The direction for ranking nodes',
            allowed = c('LR', 'TB', 'RL', 'BT')
        ),

        node_color = entity(
            name = 'Node Color',
            value = 'lightblue',
            type = 'character',
            description = 'The color of nodes in the graph',
            max_length = 1
        ),

        edge_color = entity(
            name = 'Edge Color',
            value = 'darkblue',
            type = 'character',
            description = 'The color of edges in the graph',
            max_length = 1
        ),

        edge_width = entity(
            name = 'Edge Width',
            value = 1,
            type = 'numeric',
            description = 'The width of edges in the graph',
            max_length = 1
        ),

        arrow_size = entity(
            name = 'Arrow Size',
            value = 0.5,
            type = 'numeric',
            description = 'The size of arrowheads on edges',
            max_length = 1
        ),

        arrow_shape = enum(
            name = 'Arrow Shape',
            value = 'normal',
            description = 'The shape of arrowheads',
            allowed = c('normal', 'box', 'crow', 'diamond', 'dot', 'inv',
                        'none', 'tee', 'vee')
        )
    )
)

#' @importFrom DiagrammeR create_graph add_node add_edge render_graph set_node_attrs set_edge_attrs
#' @importFrom struct param_ids param_value new_struct
#'
# Helper function to filter parameters based on show_params setting
.filter_params <- function(obj, param_names, param_values, struct_obj, node_type = NULL) {
    if (obj$show_params == "none") {
        return(character(0))
    }

    if (obj$show_params == "all") {
        return(param_names)
    }

    if (obj$show_params == "changed") {
        # Get default values using formals() for the constructor function
        class_name <- class(struct_obj)[1]
        constructor_name <- tolower(class_name)  # e.g., "pca_scores_plot" for "pca_scores_plot"

        # Try to get the constructor function
        constructor_func <- tryCatch({
            get(constructor_name, mode = "function")
        }, error = function(e) NULL)

        if (!is.null(constructor_func)) {
            # Get default values from formals
            default_formals <- formals(constructor_func)

            # Compare current values with defaults
            changed_params_list <- lapply(seq_along(param_names), function(i) {
                param_name <- param_names[i]
                current_val <- param_values[[i]]
                return(!identical(current_val, default_formals[[param_name]]))
            })

                         # Convert list to logical vector
             changed_params <- unlist(changed_params_list)
             return(param_names[changed_params])
        }
        return(param_names)  # If we can't get defaults, show all
    }

    if (obj$show_params == "connected") {
        # Check for to_param and from_param in the struct object
        connected_params <- c()
        if ("to_param" %in% param_names) {
            to_val <- param_value(struct_obj, "to_param")
            if (!is.null(to_val) && to_val != "") {
                connected_params <- c(connected_params, "to_param")
            }
        }
        if ("from_param" %in% param_names) {
            from_val <- param_value(struct_obj, "from_param")
            if (!is.null(from_val) && from_val != "") {
                connected_params <- c(connected_params, "from_param")
            }
        }
        return(connected_params)
    }

    return(character(0))
}
#'
#' @export
setMethod(f = "chart_plot",
          signature = c("model_dag_chart", "model_dag"),
          definition = function(obj, dobj) {

              # Get nodes and edges from the DAG
              nodes <- dobj$nodes
              edges <- dobj$edges

              if (length(nodes) == 0) {
                  warning("No nodes found in the DAG")
                  return(NULL)
              }

              # Create node data frame for DiagrammeR
              node_ids <- names(nodes)
              if (is.null(node_ids)) {
                  node_ids <- paste0("node_", seq_along(nodes))
              }
              # Ensure node_ids are character strings
              node_ids <- as.character(node_ids)

              # Create node labels and determine node types for coloring
              node_labels <- sapply(seq_along(nodes), function(i) {
                  node <- nodes[[i]]
                  node_name <- node_ids[i]

                  if (obj$show_params != "none" && !is.null(node)) {
                      # Handle different node types
                      params <- if (is(node, "model_node")) {
                          # For model nodes, get the actual model parameters
                          model_obj <- node$model
                          if (!is.null(model_obj) && is(model_obj, "struct_class")) {
                              param_names <- param_ids(model_obj)
                              param_values <- lapply(param_names, function(p) {
                                  val <- param_value(model_obj, p)
                                  if (is.null(val)) return("NULL")
                                  if (length(val) > 1) return('...')
                                  as.character(val)
                              })

                              # Filter parameters based on show_params setting
                              filtered_params <- .filter_params(obj, param_names, param_values, model_obj, "model")

                              if (length(filtered_params) > 0) {
                                  # Get values for filtered parameters
                                  filtered_values <- sapply(filtered_params, function(p) {
                                      idx <- which(param_names == p)
                                      param_values[idx]
                                  })
                                  # Format parameters as "param = value" on separate lines
                                  param_lines <- paste(filtered_params, filtered_values, sep = " = ", collapse = "\n")
                                  paste0(node_name, "\n", param_lines)
                              } else {
                                  paste0(node_name, "\n", class(model_obj)[1])
                              }
                          } else {
                              paste0(node_name, "\n", class(model_obj)[1])
                          }
                      } else if (is(node, "data_node")) {
                          # For data nodes, show dimensions
                          if (!is.null(node$data) && is(node$data, "DatasetExperiment")) {
                              dims <- dim(node$data$data)
                              paste0(node_name, "\n", dims[1], " x ", dims[2])
                          } else {
                              paste0(node_name, "\n", "NULL data")
                          }
                      } else if (is(node, "chart_node")) {
                          # For chart nodes, show chart info
                          if (!is.null(node$chart)) {
                              chart_obj <- node$chart
                              if (is(chart_obj, "struct_class")) {
                                  param_names <- param_ids(chart_obj)
                                  if (length(param_names) > 0) {
                                      param_values <- sapply(param_names, function(p) {
                                          val <- param_value(chart_obj, p)
                                          if (is.null(val)) return("NULL")
                                          if (length(val) > 1) return(paste(val, collapse = ", "))
                                          as.character(val)
                                      })

                                      # Filter parameters based on show_params setting
                                      filtered_params <- .filter_params(obj, param_names, param_values, chart_obj, "chart")

                                      if (length(filtered_params) > 0) {
                                          # Get values for filtered parameters
                                          filtered_values <- sapply(filtered_params, function(p) {
                                              idx <- which(param_names == p)
                                              param_values[idx]
                                          })
                                          # Format parameters as "param = value" on separate lines
                                          param_lines <- paste(filtered_params, filtered_values, sep = " = ", collapse = "\n")
                                          paste0(node_name, "\n", param_lines)
                                      } else {
                                          paste0(node_name, "\n", class(chart_obj)[1])
                                      }
                                  } else {
                                      paste0(node_name, "\n", class(chart_obj)[1])
                                  }
                              } else {
                                  paste0(node_name, "\n", class(chart_obj)[1])
                              }
                          } else {
                              paste0(node_name, "\n", "NULL chart")
                          }
                      } else if (is(node, "struct_class")) {
                          # For other struct objects, get their parameters
                          param_names <- param_ids(node)
                          param_values <- sapply(param_names, function(p) {
                              val <- param_value(node, p)
                              if (is.null(val)) return("NULL")
                              if (length(val) > 1) return(paste(val, collapse = ", "))
                              as.character(val)
                          })

                          # Filter parameters based on show_params setting
                          filtered_params <- .filter_params(obj, param_names, param_values, node, "struct")

                          if (length(filtered_params) > 0) {
                              # Get values for filtered parameters
                              filtered_values <- sapply(filtered_params, function(p) {
                                  idx <- which(param_names == p)
                                  param_values[idx]
                              })
                              # Format parameters as "param = value" on separate lines
                              param_lines <- paste(filtered_params, filtered_values, sep = " = ", collapse = "\n")
                              paste0(node_name, "\n", param_lines)
                          } else {
                              paste0(node_name, "\n", class(node)[1])
                          }
                      } else {
                          paste0(node_name, "\n", class(node)[1])
                      }

                      # Truncate if too long
                      if (nchar(params) > obj$max_param_length) {
                          params <- paste0(substr(params, 1, obj$max_param_length), "...")
                      }

                      params
                  } else {
                      node_name
                  }
              })

              # Determine node types for coloring
              node_types <- sapply(seq_along(nodes), function(i) {
                  node <- nodes[[i]]
                  if (is(node, "data_node")) {
                      "data"
                  } else if (is(node, "model_node")) {
                      "model"
                  } else if (is(node, "chart_node")) {
                      "chart"
                  } else {
                      "other"
                  }
              })

              # Create an empty graph first
              graph <- create_graph(directed = TRUE)

              # Add nodes one by one with proper labels
              for (i in seq_along(node_ids)) {
                  graph <- add_node(graph,
                                    type = "default",
                                    label = as.character(node_labels[i]))
              }

              # Add edges one by one using node IDs
              if (length(edges) > 0) {
                  for (edge in edges) {
                      if (is.list(edge) && "from" %in% names(edge) && "to" %in% names(edge)) {
                          from_val <- as.character(edge$from)
                          to_val <- as.character(edge$to)

                          # Find the node indices for the edge
                          from_idx <- which(node_ids == from_val)
                          to_idx <- which(node_ids == to_val)

                          if (length(from_idx) > 0 && length(to_idx) > 0) {
                              # Add edge using node indices (DiagrammeR uses 1-based indexing)
                              graph <- add_edge(graph,
                                                from = from_idx,
                                                to = to_idx,
                                                rel = "to")
                          }
                      }
                  }
              }

              # Set node attributes for better appearance
              # Handle auto-sizing vs fixed sizing for nodes

              if (obj$node_width != 'auto') {
                  graph <- set_node_attrs(graph, "width", obj$node_width)
              } else {
                  graph <- remove_node_attrs(graph, "height")
              }
              if (obj$node_height != 'auto') {
                  graph <- set_node_attrs(graph, "height", obj$node_height)
              } else {
                  graph <- remove_node_attrs(graph, "height")
              }
              if (obj$node_width == 'auto' || obj$node_height == 'auto') {
                  graph <- add_global_graph_attrs(graph, "fixedsize", "false", "node")
              } else {
                  graph <- add_global_graph_attrs(graph, "fixedsize", "true", "node")
              }
              graph <- set_node_attrs(graph, "fontsize", obj$font_size)
              graph <- set_node_attrs(graph, "shape", "rectangle")
              graph <- set_node_attrs(graph, "style", "filled")
              graph <- set_node_attrs(graph, "color", "black")
              graph <- set_node_attrs(graph, "penwidth", 2)

                             # Set node colors based on type using structToolbox theme colors
               # The node_types vector is in the same order as the nodes list
               # and DiagrammeR nodes are added in the same order as node_ids
               for (i in seq_along(node_types)) {
                   node_color <- switch(node_types[i],
                                        "data" = "#7fc97f",      # Green from structToolbox palette
                                        "model" = "#386cb0",     # Blue from structToolbox palette
                                        "chart" = "#fdb462",     # Orange from structToolbox palette
                                        "other" = "#a6cee3"      # Light blue from structToolbox palette
                   )
                   # Use the node index (i) to set the color - this matches the order in DiagrammeR
                   graph <- set_node_attrs(graph, "fillcolor", node_color, nodes = i)
               }

              # Set edge attributes for arrows and styling
              if (length(edges) > 0) {
                  graph <- set_edge_attrs(graph, "arrowsize", obj$arrow_size)
                  graph <- set_edge_attrs(graph, "color", obj$edge_color)
                  graph <- set_edge_attrs(graph, "penwidth", obj$edge_width)
                  graph <- set_edge_attrs(graph, "arrowhead", obj$arrow_shape)
              }

              # Render the graph with layout options
              rendered_graph <- render_graph(graph, layout = obj$layout)

              return(rendered_graph)
          }
)

# autocompletion
#' @export
#' @rdname autocompletion
#' @method .DollarNames model_dag_chart
.DollarNames.model_dag_chart <- function(x, pattern = "") {
    .DollarNames.chart(x, pattern)
}

#' @export
#' @rdname autocompletion
setMethod('.DollarNames','model_dag_chart',.DollarNames.model_dag_chart)

remove_node_attrs <- function(graph, attrs) {
    ndf <- graph$nodes_df
    w <- which(colnames(ndf) %in% attrs)
    if (length(w) > 0) {
        ndf <- ndf[, -w, drop = FALSE]
    }
    graph$nodes_df <- ndf
    graph
}
