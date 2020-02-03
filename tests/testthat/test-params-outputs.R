# test params and outputs

test_that('params and outputs',{

    test_model=function(...) {
        out = .test_model(...)
        return(out)
    }
    
    .test_model=setClass('test_model',
        contains='model',
        slots=c(
            value_1='entity',
            value_2='numeric',
            result_1='entity',
            result_2='numeric'
        ),
        prototype=list(
            predicted='result_1',
            value_1=entity(name='test'),
            result_1=entity(name='result_1',type='numeric',value=0),
            .params=c('value_1','value_2'),
            .outputs=c('result_1','result_2')
        )
    )

    ## test return objects
    # params
    obj = param_obj(test_model(),'value_1')
    expect_identical(obj,entity(name='test'))
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
    # params
    TM$value_1='cabbage'
    expect_equal(TM$value_1,'cabbage')
    expect_error(TM$value_3)
    expect_error({TM$value_3=999})

    # to test $ and $<- for outputs we need a class with outputs and NO parameters
    test_model2=setClass('test_model2',
        contains=c('model'),
        slots=c(
            result_1='entity',
            result_2='numeric'
        ),
        prototype = list(result_1=entity(type='numeric',value=1,name='result_1'),
                         .outputs=c('result_1','result_2')
        )
    )
    TM=test_model2()
    # outputs
    TM$result_1=777
    TM$result_2=555
    expect_equal(TM$result_1,777)
    expect_error(TM$result_3)
    expect_error({TM$result_3=999})

})
