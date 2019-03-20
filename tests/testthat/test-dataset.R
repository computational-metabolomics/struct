# test dataset object
test_that('dataset objects',{
    # a test dataset object
    test_data=iris_dataset()

    expect_identical(test_data$data,iris[,1:4])
    expect_identical(dataset.data(test_data),iris[,1:4])
    expect_identical(test_data$sample_meta,iris[,5,drop=FALSE])
    expect_identical(dataset.sample_meta(test_data),iris[,5,drop=FALSE])

    # check access to slots is restricted
    expect_error(test_data$cabbage)
    expect_error({test_data$cabbage='flipflop'})

    # check assignment of data
    dataset.data(test_data)=iris[,1:3]
    expect_identical(test_data$data,iris[,1:3])
    # check assignment of sample_meta
    dataset.sample_meta(test_data)=cbind(iris[,5,drop=FALSE],iris[,5,drop=FALSE])
    expect_identical(test_data$sample_meta,cbind(iris[,5,drop=FALSE],iris[,5,drop=FALSE]))
    # check assignment of variable_meta
    dataset.variable_meta(test_data)=data.frame(rownames(iris),rownames(iris))
    expect_identical(test_data$variable_meta,data.frame(rownames(iris),rownames(iris)))
    expect_identical(dataset.variable_meta(test_data),data.frame(rownames(iris),rownames(iris)))

    # check summary
    expect_output(summary(test_data),'object from the struct package')
})
