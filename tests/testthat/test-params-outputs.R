# test params and outputs

test_that('params and outputs',{

    test_model=setClass('test_model',
        contains='model',
        slots=c(
            params_value_1='entity',
            params_value_2='numeric',
            outputs_result_1='entity',
            outputs_result_2='numeric'
        ),
        prototype=list(
            predicted='result_1',
            outputs_result_1=entity(name='result_1',type='numeric',value=0))
    )

    ## test return objects
    # params
    obj = param_obj(test_model(),'value_1')
    expect_identical(obj,entity())
    obj = param_obj(test_model(),'value_2')
    expect_identical(obj,numeric(0))
    # outputs
    obj = output_obj(test_model(),'result_1')
    expect_true(is(obj,'entity'))
    obj = output_obj(test_model(),'result_2')
    expect_identical(obj,numeric(0))

    ## test set objects
    # params
    TM=test_model()
    param_obj(TM,'value_2')=5
    expect_equal(TM$value_2,5)
    # outputs
    output_obj(TM,'result_2')=5
    expect_equal(TM$result_2,5)

    ## test names
    # params
    param_obj(TM,'value_1')=entity(name='pickle')
    expect_equal(param_name(TM,'value_1'),'pickle')
    expect_equal(param_name(TM,'value_2'),'value_2')
    # outputs
    output_obj(TM,'result_1')=entity(name='carrot',type='numeric',value=10)
    expect_equal(output_name(TM,'result_1'),'carrot')
    expect_equal(output_name(TM,'result_2'),'result_2')

    ## test set lists
    L=list('value_1'='banana',value_2=20)
    param_list(TM)=L
    expect_equal(param_value(TM,'value_1'),'banana')
    expect_equal(param_value(TM,'value_2'),20)
    names(L)=c('result_1','result_2')
    L$result_1=10
    output_list(TM)=L
    expect_equal(output_value(TM,'result_1'),10)
    expect_equal(output_value(TM,'result_2'),20)

    ## get lists
    K=param_list(TM)
    expect_identical(K,list(value_1='banana',value_2=20))
    # output
    J=output_list(TM)
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
            outputs_result_1='entity',
            outputs_result_2='numeric'
        ),
        prototype = list(outputs_result_1=entity(type='numeric',value=1))
    )
    TM=test_model2()
    # outputs
    TM$result_1=777
    TM$result_2=555
    expect_equal(TM$result_1,777)
    expect_error(TM$result_3)
    expect_error({TM$result_3=999})

})
