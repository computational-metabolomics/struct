# test dataset object
test_that('DatasetExperiment objects',{
    # a test dataset object
    # NB will fail if slots cant be assigned
    test_data=iris_DatasetExperiment()

    expect_identical(colnames(test_data$data),colnames(iris[,1:4]))
    expect_identical(rownames(test_data$data),rownames(iris[,1:4]))
    expect_identical(matrix(test_data$data),matrix(iris[,1:4]))
    
    expect_identical(colnames(test_data$sample_meta),colnames(iris[,5,drop=FALSE]))
    expect_identical(rownames(test_data$sample_meta),rownames(iris[,5,drop=FALSE]))
    expect_identical(matrix(test_data$sample_meta),matrix(iris[,5,drop=FALSE]))

    # check access to slots is restricted
    expect_error(test_data$cabbage)
    expect_error({test_data$cabbage='flipflop'})

    # check show
    expect_output(show(test_data),'A "DatasetExperiment" object')
})
