### test methods

# test method objects
test_that('method objects',{

    M=method()
    expect_warning(method.apply(M,dataset())) # check default method is to do nothing and throws a warning

    # a test method object
    # adds two input values
    test_method=setClass('test_method',
        contains='method',
        slots=c(
            params.value_1='numeric',
            params.value_2='numeric',
            outputs.result_1='numeric',
            outputs.result_2='numeric'
        ),
        prototype=list(predicted='result_1')
    )

    setMethod(f='method.apply',
        signature='test_method',
        definition=function(M){
            M$result_1 = M$value_1+M$value_2
            return(M)
        })

    TM = test_method('value_1'=10,'value_2'=5)
    TM = method.apply(TM)

    expect_equal(TM$value_1,10) # check values assigned correctly
    expect_equal(TM$result_1,15)  # check method.apply
    expect_equal(predicted(TM),15) # check predicted()
    expect_equal({
        predicted.name(TM)='result_2'
        predicted.name(TM)
    },'result_2') # check method.apply
})



# test method.seq objects
test_that('method.seq objects',{

    # test data
    test_data=iris_dataset()

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
    # check indexing
    expect_identical(MS[1],test_method(value_1=1))
    # check method.steps returns the expected list
    expect_identical(method.steps(MS),list(test_method(value_1=1),test_method(value_1=2)))
    # check that we can put a list into the model
    expect_equal({
            method.steps(MS)=list(test_method(value_1=4),test_method(value_1=6))
            MS[1]$value_1
        },4)
    # check we cant add a list that contains items that are not methods
    expect_error({method.steps(MS)=list(test_method(value_1=4),dataset())})
    expect_error({method.steps(MS)=list(test_method(value_1=4),model())})

    # apply method
    MS = method.apply(MS,test_data)
    # test method chaining
    expect_identical(predicted(MS[2])$data,iris[,1:4]+10)

    # check that we can only insert a method into the sequence
    expect_error({MS[2]=dataset()})
    expect_error({MS[2]=model()})

    # check we can add a model to the front of a sequence
    MS = test_method(value_1=50) + MS
    expect_equal(MS[1]$value_1,50)
    expect_equal(MS[2]$value_1,4)
    expect_equal(MS[3]$value_1,6)

    # check we can add a model to the end of a sequence
    MS = MS + test_method(value_1=50)
    expect_equal(MS[1]$value_1,50)
    expect_equal(MS[2]$value_1,4)
    expect_equal(MS[3]$value_1,6)
    expect_equal(MS[4]$value_1,50)

    # check show
    expect_output(show(MS),'A method.seq object containing:')
    expect_output(show(method.seq()),'no methods')
})
