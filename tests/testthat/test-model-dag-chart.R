# test model_dag_chart

test_that('model_dag_chart',{
    
    # Test basic chart creation
    C = model_dag_chart()
    expect_output(show(C), regexp = 'A "model_dag_chart" object')
    
    # Test chart with custom parameters
    C = model_dag_chart(
        node_width = 3,
        node_height = 2,
        font_size = 14,
        show_params = FALSE
    )
    expect_equal(C$node_width, 3)
    expect_equal(C$node_height, 2)
    expect_equal(C$font_size, 14)
    expect_equal(C$show_params, FALSE)
    
    # Test with example DAG
    DAG = example_dag()
    expect_true(inherits(DAG, "model_dag"))
    
    # Test chart_plot method
    # Note: This will create a DiagrammeR graph object
    result = chart_plot(C, DAG)
    expect_true(!is.null(result))
    
    # Test with parameters shown
    C_with_params = model_dag_chart(show_params = TRUE)
    result_with_params = chart_plot(C_with_params, DAG)
    expect_true(!is.null(result_with_params))
    
    # Test with empty DAG (should give warning)
    empty_dag = model_dag(name = "Empty DAG", nodes = list())
    expect_warning(chart_plot(C, empty_dag))
    
    # Test autocompletion
    expect_true("node_width" %in% .DollarNames.model_dag_chart(C))
    expect_true("show_params" %in% .DollarNames.model_dag_chart(C))
    
}) 