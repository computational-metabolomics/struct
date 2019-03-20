# test stato
test_that('stato object',{

    test_class=setClass('test_Class',
        contains=c('model','stato'),
        slots=c(params.value_1='entity.stato',
             outputs.value_1='entity.stato'),
        prototype=list(stato.id='OBI:0200201',
            params.value_1=entity.stato(value=1,stato.id='OBI:0000175'),
            outputs.value_1=entity.stato(value=1,stato.id='OBI:0000175'))
        )
    S=test_class()
    expect_equal(stato.id(S),'OBI:0200201')
    expect_equal(stato.name(S),'ANOVA')
    expect_match(stato.definition(S),'ANOVA')
    expect_output(stato.summary(S),'ANOVA')
    expect_output(stato.summary(S),'p-value')
})
