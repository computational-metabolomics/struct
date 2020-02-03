# test charts

test_that('charts',{
    C=chart()
    # warning if no chart defined
    expect_warning(chart_plot(C,example_iterator()))

    ## check chart_names
    # no charts

    expect_equal(chart_names(example_iterator(),'char'),character(0))
    expect_equal(chart_names(example_iterator(),'obj'),list())
    expect_error(chart_names(example_iterator(),'banana'))

    test_chart=setClass('test_chart',
        contains='chart',
        slots=c(
            value_1='numeric'
        ),
        prototype=list(.params=c('value_1'))
    )

    setMethod(f='chart_plot',
        signature=c('test_chart','DatasetExperiment'),
        definition=function(obj,dobj){
        })

    expect_equal(chart_names(DatasetExperiment(),'char'),'test_chart')
    #expect_equal(chart_names(DatasetExperiment(),'obj'),list(test_chart()))
})
