# test struct object
test_that('struct objects can be created and modified',{
    # a test object
    test_object=struct_class(name = 'test_name', description='test_desc','type' = 'test_type')

    # name
    expect_equal({name(test_object)},'test_name') # get
    expect_equal({name(test_object)='cabbage';name(test_object)},'cabbage') # set
    #type
    expect_equal({type(test_object)},'test_type') # get
    expect_equal({type(test_object)='cabbage';type(test_object)},'cabbage') #set
    # description
    expect_equal({description(test_object)},"test_desc") #get
    expect_equal({description(test_object)='cabbage';description(test_object)},'cabbage') #set
    # show
    expect_output(show(test_object),'A struct_class object')
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
    E=entity(type='numeric',value=0)
    value(E)=1
    expect_equal(value(E),1)
})

# test enum
test_that('enum object',{
    E=enum(list=c('hello','world'),value='hello',type='character')
    # check object creation
    expect_equal(value(E),'hello')
    # check use first value if value = NULL
    E=enum(list=c('hello','world'),value=NULL)
    expect_equal(value(E),'hello')
    # check throws error if value not in list
    expect_error({value(E)='banana'},'not a valid choice for this enum')
    # check assign value
    value(E)='world'
    expect_equal(value(E),'world')
})
