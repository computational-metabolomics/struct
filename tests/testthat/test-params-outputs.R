# test params and outputs

test_that('params and outputs',{

    test_model=setClass('test_model',
        contains='model',
        slots=c(
            params.value_1='entity',
            params.value_2='numeric',
            outputs.result_1='entity',
            outputs.result_2='numeric'
        ),
        prototype=list(
            predicted='result_1',
            outputs.result_1=entity(name='result_1',type='numeric',value=0))
    )

    ## test return objects
    # params
    obj = param.obj(test_model(),'value_1')
    expect_identical(obj,entity())
    obj = param.obj(test_model(),'value_2')
    expect_identical(obj,numeric(0))
    # outputs
    obj = output.obj(test_model(),'result_1')
    expect_true(is(obj,'entity'))
    obj = output.obj(test_model(),'result_2')
    expect_identical(obj,numeric(0))

    ## test set objects
    # params
    TM=test_model()
    param.obj(TM,'value_2')=5
    expect_equal(TM$value_2,5)
    # outputs
    output.obj(TM,'result_2')=5
    expect_equal(TM$result_2,5)

    ## test names
    # params
    param.obj(TM,'value_1')=entity(name='pickle')
    expect_equal(param.name(TM,'value_1'),'pickle')
    expect_equal(param.name(TM,'value_2'),'value_2')
    # outputs
    output.obj(TM,'result_1')=entity(name='carrot',type='numeric',value=10)
    expect_equal(output.name(TM,'result_1'),'carrot')
    expect_equal(output.name(TM,'result_2'),'result_2')

    ## test set lists
    L=list('value_1'='banana',value_2=20)
    param.list(TM)=L
    expect_equal(param.value(TM,'value_1'),'banana')
    expect_equal(param.value(TM,'value_2'),20)
    names(L)=c('result_1','result_2')
    L$result_1=10
    output.list(TM)=L
    expect_equal(output.value(TM,'result_1'),10)
    expect_equal(output.value(TM,'result_2'),20)

    ## get lists
    K=param.list(TM)
    expect_identical(K,list(value_1='banana',value_2=20))
    # output
    J=output.list(TM)
    expect_identical(J,list(result_1=10,result_2=20))

    ## $ and $<-
    #params
    TM$value_1='cabbage'
    expect_equal(TM$value_1,'cabbage')
    expect_error(TM$value_3)
    expect_error({TM$value_3=999})

    # to test $ and $<- for outputs we need a class with outputs and NO parameters
    test_model2=setClass('test_model2',
        contains=c('outputs_class'),
        slots=c(
            outputs.result_1='entity',
            outputs.result_2='numeric'
        ),
        prototype = list(outputs.result_1=entity(type='numeric',value=1))
    )
    TM=test_model2()
    # outputs
    TM$result_1=777
    TM$result_2=555
    expect_equal(TM$result_1,777)
    expect_error(TM$result_3)
    expect_error({TM$result_3=999})

})
