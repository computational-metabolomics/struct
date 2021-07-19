# test struct object
test_that('ontology works as expected',{
    O=ontology_term('STATO:0000555',rols=FALSE)
    expect_true(is(O,'ontology_term'))
    expect_true(O$id=='STATO:0000555')

    # character to list
    O=ontology_list('STATO:0000555')
    expect_true(is(O,'ontology_list'))
    expect_true(O[1]$id=='STATO:0000555')
    
    # term to list
    O=ontology_list(ontology_term('STATO:0000555',rols=FALSE))
    expect_true(is(O,'ontology_list'))
    expect_true(O[1]$id=='STATO:0000555')

    # list to list
    O=ontology_list(c(
            ontology_term('STATO:0000555',rols=FALSE),
            ontology_term('STATO:0000555',rols=FALSE)
        )
    )
    expect_true(O[1]$id=='STATO:0000555')
    expect_true(O[2]$id=='STATO:0000555')
    
    # assignment
    O[2]=ontology_term('STATO:0000555',rols=FALSE)
    expect_true(O[2]$id=='STATO:0000555')
    
    # length
    expect_equal(length(O),2)
    
    # ontology list to data.frame
    df=as(O,'data.frame')
    expect_equal(nrow(df),2)
    expect_equal(ncol(df),5)
    expect_equal(df[1,1],'STATO:0000555')
    
    # catenate several ontology_lists
    O=c(O,O)
    expect_equal(length(O),4)
    expect_true(is(O,'ontology_list'))
    
    # ontology method
    M=example_model(ontology='STATO:0000555')
    expect_equal(M$ontology,'STATO:0000555')
    
    expect_error(O$cake)
    
    expect_output(show(O[1]),"STATO:0000555",regexp = 'STATO:0000555')

    O=ontology_list()
    expect_equal(length(O),0)
    
    expect_error(c(O,struct_class()))
    expect_error({O[1] = struct_class()})
    expect_error(ontology_list(terms=list(O,struct_class())))
})

