# test charts

test_that('charts',{
    C=chart()
    # warning if no chart defined
    expect_warning(chart.plot(C,example_iterator()))

    ## check chart.names
    # no charts

    expect_equal(chart.names(example_iterator(),'char'),character(0))
    expect_equal(chart.names(example_iterator(),'obj'),list())
    expect_error(chart.names(example_iterator(),'banana'))

    test_chart=setClass('test_chart',
        contains='chart',
        slots=c(
            params.value_1='numeric'
        )
    )

    setMethod(f='chart.plot',
        signature=c('test_chart','dataset'),
        definition=function(obj,dobj){
        })

    expect_equal(chart.names(dataset(),'char'),'test_chart')
    #expect_equal(chart.names(dataset(),'obj'),list(test_chart()))
})
