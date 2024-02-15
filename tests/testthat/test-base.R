# test struct object
test_that('struct objects can be created and modified',{
    # a test object
    test_object=struct_class(
        name = 'test_name',
        description=c(
            'A'='test_desc_A',
            'B'='test_desc_B'),
        type = 'test_type',
        citations= bibentry(
            bibtype ='Article',
            year = 2009,
            volume = 95,
            number = 2,
            pages = '122-128',
            author = as.person("Nestor F. Perez and Joan Ferre and Ricard Boque"),
            title = paste0('Calculation of the reliability of ',
                           'classification in discriminant partial least-squares ',
                           'binary classification'),
            journal = "Chemometrics and Intelligent Laboratory Systems"
        ),
        ontology='STATO:0000572'

    )

    # name
    expect_equal({test_object$name},'test_name') # get
    expect_equal({test_object$name='cabbage';test_object$name},'cabbage') # set
    #type
    expect_equal({test_object$type},'test_type') # get
    expect_equal({test_object$type='cabbage';test_object$type},'cabbage') #set
    # description
    expect_equal({test_object$description},c(
        'A'='test_desc_A',
        'B'='test_desc_B')) #get
    expect_output(show(test_object),'A "struct_class" object')
    expect_equal({test_object$description='cabbage';test_object$description},'cabbage') #set
    # show
    expect_output(show(test_object),'A "struct_class" object')

    expect_error({
        test_object=struct_class(
            name = 'test_name',
            description='test_desc',
            type = 'test_type',
            citations= list(
                bibentry(
                    bibtype ='Article',
                    year = 2009,
                    volume = 95,
                    number = 2,
                    pages = '122-128',
                    author = as.person("Nestor F. Perez and Joan Ferre and Ricard Boque"),
                    title = paste0('Calculation of the reliability of ',
                                   'classification in discriminant partial least-squares ',
                                   'binary classification'),
                    journal = "Chemometrics and Intelligent Laboratory Systems"
                ),
                'cake'

            ))
    })

    cit=citations(test_object)
    expect_true(length(cit)==2)
    lib=libraries(test_object)
    expect_true(length(lib)==0)
    ont=ontology(test_object,cache = list(
        'STATO:0000572'=ontology_term(
            id='STATO:0000572',
            ontology = 'stato',
            label = 'test_ontology',
            description = 'test_ontology',
            iri = 'test_ontology',
            rols=FALSE
        )
    )
    )
    expect_true(length(ont)==1)


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
    max_length(E)=1
    expect_output(show(E),regexp = 'A "entity" object')
})

# test enum
test_that('enum object',{
    E=enum(allowed=c('hello','world'),value='hello',type='character',name='test_entity')
    # check object creationS
    expect_equal(value(E),'hello')
    # check throws error if value not in list
    expect_error({value(E)='banana'},'not a valid choice for this enum')
    # check assign value
    value(E)='world'
    expect_equal(value(E),'world')
    expect_output(show(E),regexp = 'A "enum" object')
})

