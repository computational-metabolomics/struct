#' Execute a DAG object
#'
#' Executes a directed acyclic graph (DAG) object by traversing the graph
#' and executing nodes in the correct topological order.
#'
#' @param dag a model_dag object
#' @param verbose logical, whether to print progress information
#' @return the model_dag object with results included
#' @export
#' @rdname dag_execute
setGeneric("dag_execute", function(dag, verbose = TRUE) standardGeneric("dag_execute"))

#' @rdname dag_execute
#' @export
#' @examples
#' # Create a simple DAG
#' D = iris_DatasetExperiment()
#' M = PCA()
#' 
#' # Create nodes
#' data_node1 = data_node(name = 'Data', data = D)
#' model_node1 = model_node(name = 'Model', model = M)
#' 
#' # Create DAG with nodes
#' dag = model_dag(
#'     name = 'Simple DAG',
#'     description = 'A simple example DAG',
#'     edges = list(
#'         list(from = 'Data', from_param = 'asis', to = 'Model', to_param = 'input_data')
#'     ),
#'     nodes = list(
#'         'Data' = data_node1,
#'         'Model' = model_node1
#'     )
#' )
#' 
#' # Execute DAG
#' dag = dag_execute(dag)
setMethod(f = "dag_execute",
    signature = c("model_dag", "logical"),
    definition = function(dag, verbose = TRUE) {
        
        # Validate inputs
        if (!is(dag, 'model_dag')) {
            stop('dag must be a model_dag object')
        }
        
        nodes = dag$nodes
        if (!is.list(nodes) || length(nodes) == 0) {
            stop('DAG must have nodes defined')
        }
        
        # Check that all node names in edges exist in nodes
        edge_nodes = unique(c(
            unlist(lapply(dag$edges, function(e) e$from)),
            unlist(lapply(dag$edges, function(e) e$to))
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
        
        # Build adjacency list and calculate in-degrees
        adjacency = list()
        in_degrees = setNames(rep(0, length(nodes)), names(nodes))
        
        for (edge in dag$edges) {
            from = edge$from
            to = edge$to
            
            if (!(from %in% names(adjacency))) {
                adjacency[[from]] = list()
            }
            adjacency[[from]] = c(adjacency[[from]], list(to))
            in_degrees[to] = in_degrees[to] + 1
        }
        
        # Find nodes with no incoming edges (sources)
        sources = names(in_degrees)[in_degrees == 0]
        if (length(sources) == 0) {
            stop('DAG has no source nodes (nodes with no incoming edges)')
        }
        
        # Topological sort using Kahn's algorithm
        queue = sources
        processed = c()
        results = list()
        
        while (length(queue) > 0) {
            current = queue[1]
            queue = queue[-1]
            processed = c(processed, current)
            
            if (verbose) {
                cat('Processing node: ', current, '\n')
            }
            
            # Get the current node
            current_node = nodes[[current]]
            
            # Process based on node type
            if (is(current_node, 'data_node')) {
                # Data nodes: just pass the data through
                if (verbose) {
                    cat('  Data node: ', nrow(current_node@data$data), ' rows x ', 
                        ncol(current_node@data$data), ' columns\n')
                }
                results[[current]] = current_node@data
                
            } else if (is(current_node, 'model_node')) {
                # Model nodes: execute model_apply
                
                # Find input data for this node
                input_data = NULL
                
                # Look for edges pointing to this node
                for (edge in dag$edges) {
                    if (edge$to == current) {
                        from_node = edge$from
                        from_param = edge$from_param
                        to_param = edge$to_param
                        
                        if (from_node %in% names(results)) {
                            from_output = results[[from_node]]
                            from_node_obj = nodes[[from_node]]
                            
                            # Handle different from_param values
                            if (from_param == "asis" || is.null(from_param)) {
                                # Pass the object as-is (for data nodes)
                                input_data = from_output
                            } else if (from_param == "predicted") {
                                # Use predicted() output (for model nodes to other model nodes)
                                if (is(from_output, 'struct_class') && is(from_output, 'model')) {
                                    tryCatch({
                                        input_data = predicted(from_output)
                                    }, error = function(e) {
                                        stop('Failed to get predicted output from ', from_node, ': ', e$message)
                                    })
                                } else {
                                    stop('Cannot get predicted output from non-model node: ', from_node)
                                }
                            } else {
                                # Use named slot/parameter
                                if (is(from_output, 'struct_class')) {
                                    tryCatch({
                                        input_data = from_output[[from_param]]
                                    }, error = function(e) {
                                        stop('Failed to get parameter "', from_param, '" from ', from_node, ': ', e$message)
                                    })
                                } else {
                                    stop('Cannot get parameter from non-struct object: ', from_node)
                                }
                            }
                            
                            # Set the input_data for the current node
                            if (to_param == "input_data" || is.null(to_param)) {
                                current_node@input_data = input_data
                            } else {
                                # Set named parameter
                                model_obj = model(current_node)
                                model_obj[[to_param]] = input_data
                                current_node@model = model_obj
                            }
                            break
                        }
                    }
                }
                
                if (is.null(current_node@input_data)) {
                    stop('No input data found for model node: ', current)
                }
                
                # Execute the model using model_apply
                model_obj = model(current_node)
                
                if (verbose) {
                    cat('  Executing model: ', class(model_obj)[1], ' with model_apply\n')
                }
                
                # Execute the model with model_apply
                result = model_apply(model_obj, current_node@input_data)
                
                # Store the result (model_apply returns a trained model)
                results[[current]] = result
                
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
                
            } else if (is(current_node, 'prediction_node')) {
                # Prediction nodes: execute model_predict
                
                # Find input data and model for this node
                input_data = NULL
                model_obj = NULL
                
                # Look for edges pointing to this node
                for (edge in dag$edges) {
                    if (edge$to == current) {
                        from_node = edge$from
                        from_param = edge$from_param
                        to_param = edge$to_param
                        
                        if (from_node %in% names(results)) {
                            from_output = results[[from_node]]
                            from_node_obj = nodes[[from_node]]
                            
                            # Handle different from_param values
                            if (from_param == "asis" || is.null(from_param)) {
                                # Pass the object as-is (for data nodes and models to prediction nodes)
                                input_value = from_output
                            } else if (from_param == "predicted") {
                                # Use predicted() output (for model nodes to other model nodes)
                                if (is(from_output, 'struct_class') && is(from_output, 'model')) {
                                    tryCatch({
                                        input_value = predicted(from_output)
                                    }, error = function(e) {
                                        stop('Failed to get predicted output from ', from_node, ': ', e$message)
                                    })
                                } else {
                                    stop('Cannot get predicted output from non-model node: ', from_node)
                                }
                            } else {
                                # Use named slot/parameter
                                if (is(from_output, 'struct_class')) {
                                    tryCatch({
                                        input_value = from_output[[from_param]]
                                    }, error = function(e) {
                                        stop('Failed to get parameter "', from_param, '" from ', from_node, ': ', e$message)
                                    })
                                } else {
                                    stop('Cannot get parameter from non-struct object: ', from_node)
                                }
                            }
                            
                            # Handle different to_param values for prediction nodes
                            if (to_param == "input_model") {
                                model_obj = input_value
                            } else if (to_param == "input_data") {
                                input_data = input_value
                            } else {
                                # Set named parameter for the input model
                                if (is.null(model_obj)) {
                                    stop('No input model found for prediction node: ', current)
                                }
                                model_obj[[to_param]] = input_value
                            }
                        }
                    }
                }
                
                if (is.null(model_obj)) {
                    stop('No input model found for prediction node: ', current)
                }
                
                if (is.null(input_data)) {
                    stop('No input data found for prediction node: ', current)
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
                            results[[current]] = predicted_output
                        } else {
                            # Store the model result itself
                            results[[current]] = result
                        }
                    }, error = function(e) {
                        # If predicted() fails, just store the model
                        results[[current]] = result
                    })
                } else {
                    # Store the result directly
                    results[[current]] = result
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
            
            # Update in-degrees and add new sources to queue
            if (current %in% names(adjacency)) {
                for (neighbor in adjacency[[current]]) {
                    in_degrees[neighbor] = in_degrees[neighbor] - 1
                    if (in_degrees[neighbor] == 0) {
                        queue = c(queue, neighbor)
                    }
                }
            }
        }
        
        # Check for cycles
        if (length(processed) != length(nodes)) {
            stop('DAG contains cycles or unreachable nodes')
        }
        
        if (verbose) {
            cat('DAG execution completed successfully\n')
        }
        
        # Store results in the DAG object
        dag$results = results
        
        return(dag)
    }
)

