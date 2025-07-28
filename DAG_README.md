# Directed Acyclic Graphs (DAGs) for struct Models

This document describes the new DAG functionality added to the struct package, which allows you to create and execute directed acyclic graphs of struct models.

## Overview

The DAG functionality consists of three main classes:

1. **`model_dag`** - Represents a directed acyclic graph with edges defining the workflow
2. **`model_node`** - Represents a node containing a struct model and a mode function
3. **`data_node`** - Represents a node containing a DatasetExperiment object

## Classes

### model_dag

The `model_dag` class extends `struct_class` and contains an `edges` slot that defines the connections between nodes.

```r
# Create a DAG
dag = model_dag(
    name = 'My Workflow',
    description = 'A simple workflow',
    edges = list(
        list(from = 'Data', to = 'Preprocessing'),
        list(from = 'Preprocessing', to = 'Analysis')
    )
)
```

### model_node

The `model_node` class contains a struct model object and a mode function that operates on the model.

```r
# Create a model node
pca_model = PCA()
node = model_node(
    name = 'PCA Analysis',
    description = 'Principal Component Analysis',
    model = pca_model,
    mode = model_apply  # or model_train, model_predict, model_reverse
)
```

### data_node

The `data_node` class contains a DatasetExperiment object that serves as input to other nodes.

```r
# Create a data node
D = iris_DatasetExperiment()
data_node = data_node(
    name = 'My Data',
    description = 'Iris dataset',
    data = D
)

# Access the data
data_value(data_node)
```

## Execution

The `dag_execute` function executes a DAG by:

1. Validating the DAG structure
2. Performing topological sorting to determine execution order
3. Executing nodes in the correct order
4. Passing outputs between nodes according to the edges

```r
# Execute a DAG
nodes = list(
    'Data' = data_node,
    'Preprocessing' = preprocessing_node,
    'Analysis' = analysis_node
)
results = dag_execute(dag, nodes, verbose = TRUE)
```

## Example Workflows

### Simple Preprocessing Workflow

```r
# Load data
D = iris_DatasetExperiment()

# Create nodes
data_node = data_node(name = 'Data', data = D)
mean_center_node = model_node(
    name = 'Mean Centering',
    model = mean_centre(),
    mode = model_apply
)
pca_node = model_node(
    name = 'PCA',
    model = PCA(),
    mode = model_apply
)

# Create DAG
dag = model_dag(
    name = 'Preprocessing Workflow',
    edges = list(
        list(from = 'Data', to = 'Mean Centering'),
        list(from = 'Mean Centering', to = 'PCA')
    )
)

# Execute
nodes = list(
    'Data' = data_node,
    'Mean Centering' = mean_center_node,
    'PCA' = pca_node
)
results = dag_execute(dag, nodes)
```

### Complex Workflow with Parallel Paths

```r
# Create a workflow with parallel PCA and PLS analysis
dag = model_dag(
    name = 'Complex Analysis',
    edges = list(
        list(from = 'Data', to = 'Preprocessing'),
        list(from = 'Preprocessing', to = 'PCA Train'),
        list(from = 'Preprocessing', to = 'PLS Train'),
        list(from = 'PCA Train', to = 'PCA Predict'),
        list(from = 'PLS Train', to = 'PLS Predict')
    )
)
```

## Available Modes

The following modes can be used with model nodes:

- `model_apply` - Train and apply the model in one step
- `model_train` - Train the model only
- `model_predict` - Apply a trained model
- `model_reverse` - Apply the reverse transformation

## Validation

The DAG execution includes several validation checks:

1. Ensures all nodes referenced in edges exist
2. Validates that all nodes are of the correct type
3. Checks for cycles in the graph
4. Ensures all model nodes have input data

## Error Handling

The DAG execution provides informative error messages for common issues:

- Missing nodes referenced in edges
- Invalid node types
- Cycles in the graph
- Missing input data for model nodes

## Benefits

The DAG functionality provides several benefits:

1. **Modularity** - Each step is encapsulated in its own node
2. **Reusability** - Nodes can be reused in different workflows
3. **Clarity** - The workflow structure is explicitly defined
4. **Validation** - Automatic validation of workflow structure
5. **Flexibility** - Support for complex workflows with parallel paths 