### test models

# test model objects
test_that('model objects',{

    M=model()
    expect_warning(model.train(M,dataset())) # check default model is to do nothing and throws a warning
    expect_identical(model.predict(M,dataset()),M) # check default prediction is nothing

    # a test model object
    # adds two input values
    test_model=setClass('test_model',
        contains='model',
        slots=c(
            params.value_1='numeric',
            params.value_2='numeric',
            outputs.result_1='numeric',
            outputs.result_2='numeric'
        ),
        prototype=list(predicted='result_1')
    )

    setMethod(f='model.train',
        signature='test_model',
        definition=function(M){
            M$result_1 = M$value_1+M$value_2
            return(M)
        })

    setMethod(f='model.predict',
        signature='test_model',
        definition=function(M){
            M$result_2 = M$value_1/M$value_2
            return(M)
        })

    TM = test_model('value_1'=10,'value_2'=5)

    TM = model.train(TM)
    TM = model.predict(TM);

    expect_equal(TM$value_1,10) # check values assigned correctly
    expect_equal(TM$result_1,15)  # check model.train
    expect_equal(TM$result_2,2)  # check model.predict
    expect_equal(predicted(TM),15) # check predicted()
    expect_equal({
        predicted.name(TM)='result_2'
        predicted.name(TM)
    },'result_2') # check predicted.name<-
})



