# test struct object
test_that('struct objects can be created and modified',{
    # a test object
    test_object=struct_class(name = 'test_name', description='test_desc','type' = 'test_type')

    # name
    expect_equal({test_object$name},'test_name') # get
    expect_equal({test_object$name='cabbage';test_object$name},'cabbage') # set
    #type
    expect_equal({test_object$type},'test_type') # get
    expect_equal({test_object$type='cabbage';test_object$type},'cabbage') #set
    # description
    expect_equal({test_object$description},"test_desc") #get
    expect_equal({test_object$description='cabbage';test_object$description},'cabbage') #set
    # show
    expect_output(show(test_object),'A "struct_class" object\\n-----------------------\\nname:          cabbage\\ndescription:   cabbage')
})

# test metric object
test_that('metric object',{
    M=metric()
    expect_warning(calculate(M,1,1))
    # return value
    expect_equal(value(M),numeric(0))
})

# test entity
test_that('entity object',{
    E=entity(type='numeric',value=0,name='test_enti')
    value(E)=1
    expect_equal(value(E),1)
})

# test enum
test_that('enum object',{
    E=enum(allowed=c('hello','world'),value='hello',type='character',name='test_entity')
    # check object creation
    expect_equal(value(E),'hello')
    # check throws error if value not in list
    expect_error({value(E)='banana'},'not a valid choice for this enum')
    # check assign value
    value(E)='world'
    expect_equal(value(E),'world')
})
