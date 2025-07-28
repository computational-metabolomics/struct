#' Execute a DAG object
#'
#' Executes a directed acyclic graph (DAG) object by traversing the graph
#' and executing nodes in the correct topological order.
#'
#' @param dag a model_dag object
#' @param nodes a named list of nodes (model_node, data_node, and prediction_node objects) where names are node IDs
#' @param verbose logical, whether to print progress information
#' @return a list containing the results of each node execution
#' @export
#' @rdname dag_execute
setGeneric("dag_execute", function(dag, nodes, verbose = TRUE) standardGeneric("dag_execute"))

#' @rdname dag_execute
#' @export
#' @examples
#' # Create a simple DAG
#' D = iris_DatasetExperiment()
#' M = example_model()
#' 
#' # Create nodes
#' data_node1 = data_node(name = 'Data', data = D)
#' model_node1 = model_node(name = 'Model', model = M)
#' 
#' # Create DAG
#' dag = model_dag(
#'     name = 'Simple DAG',
#'     description = 'A simple example DAG',
#'     edges = list(
#'         list(from = 'Data', to = 'Model')
#'     )
#' )
#' 
#' # Execute DAG
#' nodes = list(Data = data_node1, Model = model_node1)
#' results = dag_execute(dag, nodes)
#' @return a list containing the results of each node execution
setMethod(f = "dag_execute",
    signature = c("model_dag", "list", "logical"),
    definition = function(dag, nodes, verbose = TRUE) {
    
    # Validate inputs
    if (!is(dag, 'model_dag')) {
        stop('dag must be a model_dag object')
    }
    
    if (!is.list(nodes) || length(nodes) == 0) {
        stop('nodes must be a non-empty list')
    }
    
    # Check that all node names in edges exist in nodes
    edge_nodes = unique(c(
        unlist(lapply(dag@edges, function(e) e$from)),
        unlist(lapply(dag@edges, function(e) e$to))
    ))
    
    missing_nodes = setdiff(edge_nodes, names(nodes))
    if (length(missing_nodes) > 0) {
        stop('The following nodes referenced in edges are missing from nodes: ', 
             paste(missing_nodes, collapse = ', '))
    }
    
    # Check that all nodes are valid
    for (node_name in names(nodes)) {
        node = nodes[[node_name]]
        if (!is(node, 'model_node') && !is(node, 'data_node') && !is(node, 'prediction_node')) {
            stop('Node "', node_name, '" must be a model_node, data_node, or prediction_node object')
        }
    }
    
    # Build adjacency list for topological sorting
    adjacency = list()
    in_degree = list()
    
    # Initialize
    for (node_name in names(nodes)) {
        adjacency[[node_name]] = character(0)
        in_degree[[node_name]] = 0
    }
    
    # Build adjacency list and calculate in-degrees
    for (edge in dag@edges) {
        from = edge$from
        to = edge$to
        
        if (!(from %in% names(adjacency))) {
            adjacency[[from]] = character(0)
            in_degree[[from]] = 0
        }
        if (!(to %in% names(adjacency))) {
            adjacency[[to]] = character(0)
            in_degree[[to]] = 0
        }
        
        adjacency[[from]] = c(adjacency[[from]], to)
        in_degree[[to]] = in_degree[[to]] + 1
    }
    
    # Topological sort using Kahn's algorithm
    queue = character(0)
    for (node_name in names(in_degree)) {
        if (in_degree[[node_name]] == 0) {
            queue = c(queue, node_name)
        }
    }
    
    topo_order = character(0)
    while (length(queue) > 0) {
        current = queue[1]
        queue = queue[-1]
        topo_order = c(topo_order, current)
        
        for (neighbor in adjacency[[current]]) {
            in_degree[[neighbor]] = in_degree[[neighbor]] - 1
            if (in_degree[[neighbor]] == 0) {
                queue = c(queue, neighbor)
            }
        }
    }
    
    # Check for cycles
    if (length(topo_order) != length(names(nodes))) {
        stop('DAG contains cycles or disconnected components')
    }
    
    if (verbose) {
        cat('Executing DAG in order: ', paste(topo_order, collapse = ' -> '), '\n')
    }
    
    # Execute nodes in topological order
    results = list()
    node_outputs = list()
    
    for (node_name in topo_order) {
        node = nodes[[node_name]]
        
        if (verbose) {
            cat('Executing node: ', node_name, ' (', class(node)[1], ')\n')
        }
        
        if (is(node, 'data_node')) {
            # 1. If DataNode do nothing, set data as input to connected step
            node_outputs[[node_name]] = data_value(node)
            results[[node_name]] = node_outputs[[node_name]]
            
            if (verbose) {
                cat('  Data node output: ', nrow(node_outputs[[node_name]]$data), 
                    ' rows x ', ncol(node_outputs[[node_name]]$data), ' columns\n')
            }
            
        } else if (is(node, 'model_node')) {
            # 2. If model_node, determine whether to use model_apply or model_train based on connections
            
            # Find input data for this node
            input_data = NULL
            
            # Look for edges pointing to this node
            for (edge in dag@edges) {
                if (edge$to == node_name) {
                    from_node = edge$from
                    if (from_node %in% names(node_outputs)) {
                        from_output = node_outputs[[from_node]]
                        
                        # a) If the from_node output is a DatasetExperiment, use it as input
                        if (is(from_output, 'DatasetExperiment')) {
                            input_data = from_output
                            break
                        } else if (is(from_output, 'struct_class') && is(from_output, 'model')) {
                            # b) If it's a model, get its predicted output
                            tryCatch({
                                predicted_output = predicted(from_output)
                                if (is(predicted_output, 'DatasetExperiment')) {
                                    input_data = predicted_output
                                    break
                                }
                            }, error = function(e) {
                                # If predicted() fails, we can't use this as input
                            })
                        }
                    }
                }
            }
            
            if (is.null(input_data)) {
                stop('No input data found for model node: ', node_name)
            }
            
            # Execute the model using model_apply
            model_obj = model(node)
            
            if (verbose) {
                cat('  Executing model: ', class(model_obj)[1], ' with model_apply\n')
            }
            # Execute the model with model_apply
            result = model_apply(model_obj, input_data)
            
            # Store the result (model_apply returns a trained model)
            if (is(result, 'struct_class') && is(result, 'model')) {
                # Store the trained model for the next node
                node_outputs[[node_name]] = result
                results[[node_name]] = result
            } else {
                # Store the result directly
                node_outputs[[node_name]] = result
                results[[node_name]] = result
            }
            
            if (verbose) {
                if (is(result, 'struct_class') && is(result, 'model')) {
                    tryCatch({
                        predicted_output = predicted(result)
                        if (is(predicted_output, 'DatasetExperiment')) {
                            cat('  Model output: ', nrow(predicted_output$data), ' rows x ', 
                                ncol(predicted_output$data), ' columns\n')
                        } else {
                            cat('  Model output: ', class(predicted_output)[1], '\n')
                        }
                    }, error = function(e) {
                        # If predicted() fails, just show the model class
                        cat('  Model output: trained ', class(result)[1], ' model\n')
                    })
                } else if (is(result, 'DatasetExperiment')) {
                    cat('  Model output: ', nrow(result$data), ' rows x ', 
                        ncol(result$data), ' columns\n')
                } else {
                    cat('  Model output: ', class(result)[1], '\n')
                }
            }
            
        } else if (is(node, 'prediction_node')) {
            # 3. If a prediction_node, then using model_predict with the connected model and data
            
            # Find input data and model for this node
            input_data = NULL
            model_obj = NULL
            
            # Look for edges pointing to this node
            for (edge in dag@edges) {
                if (edge$to == node_name) {
                    from_node = edge$from
                    if (from_node %in% names(node_outputs)) {
                        from_output = node_outputs[[from_node]]
                        from_node_obj = nodes[[from_node]]
                        
                        if (is(from_node_obj, 'model_node')) {
                            # Input from model_node: use the trained model
                            if (is(from_output, 'struct_class') && is(from_output, 'model')) {
                                model_obj = from_output
                            }
                        } else if (is(from_node_obj, 'data_node')) {
                            # Input from data_node: use the DatasetExperiment
                            if (is(from_output, 'DatasetExperiment')) {
                                input_data = from_output
                            }
                        } else if (is(from_node_obj, 'prediction_node')) {
                            # Input from prediction_node: use the predicted output
                            if (is(from_output, 'DatasetExperiment')) {
                                input_data = from_output
                            }
                        }
                    }
                }
            }
            
            if (is.null(model_obj)) {
                stop('No trained model found for prediction node: ', node_name)
            }
            
            if (is.null(input_data)) {
                stop('No input data found for prediction node: ', node_name)
            }
            
            if (verbose) {
                cat('  Executing model: ', class(model_obj)[1], ' with model_predict\n')
            }
            
            # Execute the model with model_predict
            result = model_predict(model_obj, input_data)
            
            # Get the predicted output
            if (is(result, 'struct_class') && is(result, 'model')) {
                tryCatch({
                    predicted_output = predicted(result)
                    if (is(predicted_output, 'DatasetExperiment')) {
                        # Store the predicted output for the next node
                        node_outputs[[node_name]] = predicted_output
                        results[[node_name]] = result
                    } else {
                        # Store the model result itself
                        node_outputs[[node_name]] = result
                        results[[node_name]] = result
                    }
                }, error = function(e) {
                    # If predicted() fails, just store the model
                    node_outputs[[node_name]] = result
                    results[[node_name]] = result
                })
            } else {
                # Store the result directly
                node_outputs[[node_name]] = result
                results[[node_name]] = result
            }
            
            if (verbose) {
                if (is(result, 'struct_class') && is(result, 'model')) {
                    tryCatch({
                        predicted_output = predicted(result)
                        if (is(predicted_output, 'DatasetExperiment')) {
                            cat('  Model output: ', nrow(predicted_output$data), ' rows x ', 
                                ncol(predicted_output$data), ' columns\n')
                        } else {
                            cat('  Model output: ', class(predicted_output)[1], '\n')
                        }
                    }, error = function(e) {
                        # If predicted() fails, just show the model class
                        cat('  Model output: trained ', class(result)[1], ' model\n')
                    })
                } else if (is(result, 'DatasetExperiment')) {
                    cat('  Model output: ', nrow(result$data), ' rows x ', 
                        ncol(result$data), ' columns\n')
                } else {
                    cat('  Model output: ', class(result)[1], '\n')
                }
            }
        }
    }
    
    if (verbose) {
        cat('DAG execution completed successfully\n')
    }
    
    return(results)
}
)

#' @rdname dag_execute
#' @export
setMethod(f = "dag_execute",
    signature = c("model_dag", "list", "missing"),
    definition = function(dag, nodes, verbose = TRUE) {
        return(dag_execute(dag, nodes, verbose = TRUE))
    }
)