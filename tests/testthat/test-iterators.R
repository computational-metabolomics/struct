## test iterators
test_that('iterator objects',{

    I=iterator()
    MET=metric()
    D=DatasetExperiment()

    # check warning if not implemented
    expect_warning(run(I,D,MET))
    # check evaluate
    expect_warning(evaluate(I,MET))

    # test get/set model
    models(I)=model()
    expect_equal(models(I),model_seq()+model())

    # object for testing
    test_iterator=setClass('test_iterator',
        contains='iterator',
        slots=c(
            value_1='numeric',
            value_2='numeric',
            result_1='numeric',
            result_2='numeric'
        ),
        prototype = list(
            .params=c('value_1','value_2'),
            .outputs=c('result_1','result_2')
        )
    )

    setMethod(f='run',
        signature=c('test_iterator','DatasetExperiment','metric'),
        definition=function(I,D,MET){
            I$result_1 = 3.142
            return(I)
        })

    I=test_iterator()

    # test result()
    result_name(I)='result_1'
    expect_equal(result_name(I),'result_1')


    # test run iterator
    I=run(I,D,MET)
    expect_equal(I$result_1,3.142)
    expect_equal(result(I),3.142)

    # test combine with models and iterators
    IM=I*model()
    expect_equal(models(IM),model_seq()+model())
    IIM=I*IM
    expect_equal(models(IIM),IM)
    IIM=I*I*model()
    expect_equal(models(IIM),IM)

    # check we get an error if incorrectly combined
    expect_error(model()*IM)
    
    O=optimiser(result='name')
    expect_true(is(O,'optimiser'))
    
    R=resampler(result='name')
    expect_true(is(R,'resampler'))
    
    P=preprocess(predicted='name')
    expect_true(is(P,'preprocess'))
    
    expect_output(show(R),regexp = 'A "resampler" object')
    expect_output(cat(as.code(R*example_model(),mode='compact')),regexp='M = resampler')
    expect_output(cat(as.code(R*(example_model()+example_model()),mode='compact')),regexp='M = resampler')
    expect_output(cat(as.code(R*(example_model()+example_model()),mode='expanded')),regexp='M = resampler')

    expect_output(show(O),regexp = 'A "optimiser" object')
    expect_output(cat(as.code(O*example_model())),regexp='M = optimiser')
    
    expect_output(show(P),regexp = 'A "preprocess" object')
    expect_output(cat(as.code(P),regexp='M = preprocess'))
    
    expect_true("name" %in% .DollarNames.iterator(I))
    expect_true("name" %in% .DollarNames.optimiser(O))
    expect_true("name" %in% .DollarNames.resampler(R))
    expect_true("name" %in% .DollarNames.preprocess(P))
})
