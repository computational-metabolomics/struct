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
    v=data.frame('sample_id'=rownames(iris))
    rownames(v)=rownames(iris)
    test_data = dataset(
        name = 'Iris',
        description = "Fisher's Iris data",
        type='single_block',
        data=iris[,1:4],
        sample_meta=iris[,5,drop=FALSE],
        variable_meta=v
    )

    expect_identical(test_data$data,iris[,1:4])
    expect_identical(dataset.data(test_data),iris[,1:4])
    expect_identical(test_data$sample_meta,iris[,5,drop=FALSE])
    expect_identical(dataset.sample_meta(test_data),iris[,5,drop=FALSE])
})

# test method objects
test_that('method objects',{

    # a test method object
    # adds two input values
    test_method=setClass('test_method',
        contains='method',
        slots=c(
            params.value_1='numeric',
            params.value_2='numeric',
            outputs.result='numeric'
        ),
        prototype=list(predicted='result')
    )

    setMethod(f='method.apply',
        signature='test_method',
        definition=function(M){
            M$result = M$value_1+M$value_2
            return(M)
        })

    TM = test_method('value_1'=10,'value_2'=5)

    TM = method.apply(TM)

    expect_equal(TM$value_1,10) # check values assigned correctly
    expect_equal(TM$result,15)  # check method.apply
    expect_equal(predicted(TM),15) # check predicted()

})

# test model objects
test_that('model objects',{

    # a test model object
    # adds two input values
    test_model=setClass('test_model',
        contains='model',
        slots=c(
            params.value_1='numeric',
            params.value_2='numeric',
            outputs.train_result='numeric',
            outputs.test_result='numeric'
        ),
        prototype=list(predicted='test_result')
    )

    setMethod(f='model.train',
        signature='test_model',
        definition=function(M){
            M$train_result = M$value_1+M$value_2 # training adds
            return(M)
        })

    setMethod(f='model.predict',
        signature='test_model',
        definition=function(M){
            M$test_result = M$value_1/M$value_2 # prediction divides
            return(M)
        })

    TM = test_model('value_1'=10,'value_2'=5)

    TM = model.train(TM)
    TM = model.predict(TM)

    expect_equal(TM$value_1,10) # check values assigned correctly
    expect_equal(TM$train_result,15) # check model.train
    expect_equal(TM$test_result,2) # check model.predict
    expect_equal(predicted(TM),2) # check predicted()
})

# test method.seq objects
test_that('method.seq objects',{
    # a test dataset object
    v=data.frame('sample_id'=rownames(iris))
    rownames(v)=rownames(iris)
    test_data = dataset(
        name = 'Iris',
        description = "Fisher's Iris data",
        type='single_block',
        data=iris[,1:4],
        sample_meta=iris[,5,drop=FALSE],
        variable_meta=v
    )

    # a test method object
    test_method=setClass('test_method',
        contains='method',
        slots=c(
            params.value_1='numeric',
            outputs.result='dataset'
        ),
        prototype=list(predicted='result')
    )

    setMethod(f='method.apply',
        signature=c('test_method','dataset'),
        definition=function(M,D){
            D$data=D$data+M$value_1
            M$result = D
            return(M)
        })

    MS = test_method(value_1=1) + test_method(value_1=2)
    expect_identical(MS[1],test_method(value_1=1)) # check indexing

    MS = method.apply(MS,test_data)
    expect_identical(predicted(MS[2])$data,iris[,1:4]+3) # test method chain

})
