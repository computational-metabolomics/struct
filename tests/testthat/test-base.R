# test struct object
test_that('struct objects can be created and modified',{
    # a test object
    test_object=struct_class(name = 'test_name', description='test_desc','type' = 'test_type')

    # name
    expect_equal({name(test_object)},'test_name') # get
    expect_equal({name(test_object)='cabbage';name(test_object)},'cabbage') # set
    #type
    expect_equal({type(test_object)},'test_type') # get
    expect_equal({type(test_object)='cabbage';type(test_object)},'cabbage') #set
    # description
    expect_equal({description(test_object)},"test_desc") #get
    expect_equal({description(test_object)='cabbage';description(test_object)},'cabbage') #set
})


# test dataset object
test_that('dataset objects',{
    # a test dataset object
    test_data=iris_dataset()

    expect_identical(test_data$data,iris[,1:4])
    expect_identical(dataset.data(test_data),iris[,1:4])
    expect_identical(test_data$sample_meta,iris[,5,drop=FALSE])
    expect_identical(dataset.sample_meta(test_data),iris[,5,drop=FALSE])
})


