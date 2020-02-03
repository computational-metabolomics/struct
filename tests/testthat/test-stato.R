# test stato
test_that('stato object',{

    test_class=setClass('test_Class',
        contains=c('model','stato'),
        slots=c(
            value_1='entity_stato',
            result_1='entity_stato'),
        prototype=list(
            stato_id='OBI:0200201',
            value_1=entity_stato(value=1,stato_id='OBI:0000175',type='numeric',name='value_1'),
            result_1=entity_stato(value=1,stato_id='OBI:0000175',type='numeric',name='result_1'))
        )
    S=test_class()
    expect_equal(stato_id(S),'OBI:0200201')
    expect_equal(stato_name(S),'ANOVA')
    expect_match(stato_definition(S),'ANOVA')
    expect_output(stato_summary(S),'ANOVA')
    expect_output(stato_summary(S),'OBI:0200201')
})
