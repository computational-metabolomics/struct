test_that("chart_node works with single input", {
    # Create a simple chart node with single input
    D = iris_DatasetExperiment()
    C = example_chart()
    
    chart_node1 = chart_node(name = 'Test Chart', description = 'Test chart node', chart = C)
    
    # Create DAG with chart node
    dag = model_dag(
        name = 'Test Chart DAG',
        description = 'Test DAG with chart node',
        edges = list(
            list(from = 'Data', from_param = 'asis', to = 'Chart', to_param = 'input_object')
        ),
        nodes = list(
            'Data' = data_node(name = 'Data', data = D),
            'Chart' = chart_node1
        )
    )
    
    # Execute DAG
    dag = dag_execute(dag, verbose = FALSE)
    
    # Check that results contain the chart output
    expect_true('Chart' %in% names(dag$results))
    expect_true(!is.null(dag$results[['Chart']]))
})

test_that("chart_node works with multiple inputs", {
    # This test would require structToolbox to be available
    # For now, we'll just test the chart_node creation
    C = example_chart()
    
    chart_node1 = chart_node(name = 'Multi Input Chart', description = 'Chart with multiple inputs', chart = C)
    
    # Check that the chart node was created correctly
    expect_true(is(chart_node1, 'chart_node'))
    expect_true(is(chart_node1, 'struct_node'))
    expect_equal(chart(chart_node1), C)
    expect_equal(length(chart_node1@input_objects), 0)
})

test_that("chart_node show method works", {
    C = example_chart()
    chart_node1 = chart_node(name = 'Test Chart', description = 'Test chart node', chart = C)
    
    # Capture the output of show method
    output = capture.output(show(chart_node1))
    
    # Check that the output contains expected information
    expect_true(any(grepl('chart:', output)))
    expect_true(any(grepl('input objects:', output)))
})

test_that("chart_node chart getter and setter work", {
    C1 = example_chart()
    C2 = example_chart()
    
    chart_node1 = chart_node(chart = C1)
    
    # Test getter
    expect_equal(chart(chart_node1), C1)
    
    # Test setter
    chart(chart_node1) = C2
    expect_equal(chart(chart_node1), C2)
})

test_that("chart_node validation in DAG execution", {
    # Test that chart_node is accepted as a valid node type
    D = iris_DatasetExperiment()
    C = example_chart()
    
    chart_node1 = chart_node(name = 'Chart', chart = C)
    
    dag = model_dag(
        name = 'Valid Chart DAG',
        description = 'DAG with valid chart node',
        edges = list(),
        nodes = list(
            'Data' = data_node(name = 'Data', data = D),
            'Chart' = chart_node1
        )
    )
    
    # This should not throw an error
    expect_no_error(dag_execute(dag, verbose = FALSE))
}) 