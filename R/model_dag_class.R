#' model_dag class
#'
#' A class for representing directed acyclic graphs (DAGs) of struct models.
#' This class extends struct_class and provides functionality for executing
#' workflows defined as directed acyclic graphs.
#'
#' @section \code{edges} slot:
#' The "edges" slot is a list that defines the connections between nodes in the DAG.
#' Each edge should be a list with 'from' and 'to' elements specifying the source
#' and target nodes.
#'
#' @export model_dag
#' @param name the name of the DAG
#' @param description a description of the DAG
#' @param edges a list of edges defining the DAG structure
#' @param ... additional parameters to pass to struct_class
#' @include generics.R struct_class.R
#' @examples
#' DAG = model_dag(name = 'Example DAG', description = 'A simple example DAG')
#' @rdname model_dag
model_dag = function(name = character(0), description = character(0), edges = list(), nodes = list(), results = list(), ...) {
    # new object
    out = new_struct('model_dag',
        name = name,
        description = description,
        edges = edges,
        nodes = nodes,
        results = results,
        ...)
    return(out)
}

.model_dag <- setClass(
    "model_dag",
    contains = c('struct_class'),
    slots = c(
        edges = 'entity',
        nodes = 'entity',
        results = 'entity'
    ),
    prototype = list(
        name = 'Model DAG',
        description = 'A directed acyclic graph of struct models',
        type = 'dag',
        .params = c('edges', 'nodes', 'results'),
        
        edges = entity(
            name = 'Edges',
            value = list(),
            type = 'list',
            description = 'List of edges defining the DAG structure',
            max_length = Inf
        ),
        
        nodes = entity(
            name = 'Nodes',
            value = list(),
            type = 'list',
            description = 'List of nodes (data_node, model_node, prediction_node objects)',
            max_length = Inf
        ),
        
        results = entity(
            name = 'Results',
            value = list(),
            type = 'list',
            description = 'Results of DAG execution',
            max_length = Inf
        )
    )
)









# autocompletion
#' @export
#' @rdname autocompletion
#' @method .DollarNames model_dag
.DollarNames.model_dag <- function(x, pattern = "") {
    .DollarNames.struct_class(x, pattern)
}

#' @export 
#' @rdname autocompletion
setMethod('.DollarNames','model_dag',.DollarNames.model_dag)



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
#' # Create DAG with nodes
#' dag = model_dag(
#'     name = 'Simple DAG',
#'     description = 'A simple example DAG',
#'     edges = list(
#'         list(from = 'Data', to = 'Model')
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
        
        # Build adjacency list for topological sorting
        adjacency = list()
        in_degree = list()
        
        # Initialize
        for (node_name in names(nodes)) {
            adjacency[[node_name]] = character(0)
            in_degree[[node_name]] = 0
        }
        
        # Build adjacency list and calculate in-degrees
        for (edge in dag$edges) {
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
        node_outputs = list()
        results = list()
        
        for (node_name in topo_order) {
            node = nodes[[node_name]]
            
            if (verbose) {
                cat('Executing node: ', node_name, ' (', class(node)[1], ')\n')
            }
            
            if (is(node, 'data_node')) {
                # Data node: pass through the data
                result = node@data
                node_outputs[[node_name]] = result
                results[[node_name]] = result
                
                if (verbose) {
                    if (is(result, 'DatasetExperiment')) {
                        cat('  Data output: ', nrow(result$data), ' rows x ', 
                            ncol(result$data), ' columns\n')
                    } else {
                        cat('  Data output: ', class(result)[1], '\n')
                    }
                }
                
            } else if (is(node, 'model_node')) {
                # Model node: find input data and execute model_apply
                input_data = NULL
                
                            # Find input data from incoming edges
            for (edge in dag$edges) {
                    if (edge$to == node_name) {
                        from_node_name = edge$from
                        if (from_node_name %in% names(node_outputs)) {
                            input_data = node_outputs[[from_node_name]]
                            break
                        }
                    }
                }
                
                if (is.null(input_data)) {
                    stop('No input data found for model node: ', node_name)
                }
                
                if (verbose) {
                    cat('  Executing model: ', class(node@model)[1], ' with model_apply\n')
                }
                
                # Execute the model with model_apply
                result = model_apply(node@model, input_data)
                
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
                
            } else if (is(node, 'prediction_node')) {
                # Prediction node: find trained model and input data
                model_obj = NULL
                input_data = NULL
                
                            # Find inputs from incoming edges
            for (edge in dag$edges) {
                    if (edge$to == node_name) {
                        from_node_name = edge$from
                        if (from_node_name %in% names(node_outputs)) {
                            from_output = node_outputs[[from_node_name]]
                            from_node_obj = nodes[[from_node_name]]
                            
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
        
            # Store results in the DAG object
    dag$results = results
    
    return(dag)
    }
) 