# test stato
test_that('stato object',{

    test_class=setClass('test_Class',
        contains=c('model','stato'),
        slots=c(
            value_1='entity_stato',
            value_2='enum_stato',
            result_1='entity_stato'),
        prototype=list(
            stato_id='OBI:0200201',
            value_1=entity_stato(value=1,stato_id='OBI:0000175',type='numeric',name='value_1'),
            value_2=enum_stato(value=1,stato_id='OBI:0000175',type='numeric',allowed=c(1,2),name='value_2'),
            result_1=entity_stato(value=1,stato_id='OBI:0000175',type='numeric',name='result_1'),
        .params=c('value_1','value_2'),
        .outputs=c('result_1')
        ))
    S=test_class()
    expect_equal(suppressWarnings(stato_id(S)),'OBI:0200201')
    expect_equal(suppressWarnings(stato_name(S)),'ANOVA')
    expect_match(suppressWarnings(stato_definition(S)),'ANOVA')
    expect_output(suppressWarnings(stato_summary(S)),'ANOVA')
    expect_output(suppressWarnings(stato_summary(S)),'OBI:0200201')
    
    obj=param_obj(S,'value_1')
    expect_output(suppressWarnings(show(obj)),regexp = 'Stato ID')
    
    obj=param_obj(S,'value_2')
    expect_output(suppressWarnings(show(obj)),regexp = 'Stato ID')
    
})
