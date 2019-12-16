### test models

# test model objects
test_that('model objects',{

    M=model()
    expect_warning(model_train(M,DatasetExperiment())) # check default model is to do nothing and throws a warning
    expect_identical(model_predict(M,DatasetExperiment()),M) # check default prediction is nothing

    # a test model object
    # adds two input values
    test_model=function(...) {
        out = .test_model()
        out = .initialize_struct_class(out,...)
        return(out)
    }
    
    .test_model=setClass('test_model',
                        contains='model',
                        slots=c(
                            params_value_1='numeric',
                            params_value_2='numeric',
                            outputs_result_1='numeric',
                            outputs_result_2='numeric'
                        ),
                        prototype=list(predicted='result_1')
    )

    setMethod(f='model_train',
              signature=c('test_model','DatasetExperiment'),
              definition=function(M,D){
                  M$result_1 = M$value_1+M$value_2
                  return(M)
              })

    setMethod(f='model_predict',
              signature=c('test_model','DatasetExperiment'),
              definition=function(M,D){
                  M$result_2 = M$value_1/M$value_2
                  return(M)
              })

    TM = test_model('value_1'=10,'value_2'=5)

    TM = model_train(TM,DatasetExperiment())
    TM = model_predict(TM,DatasetExperiment())

    expect_equal(TM$value_1,10) # check values assigned correctly
    expect_equal(TM$result_1,15)  # check model_train
    expect_equal(TM$result_2,2)  # check model_predict
    expect_equal(predicted(TM),15) # check predicted()
    expect_equal({
        predicted_name(TM)='result_2'
        predicted_name(TM)
    },'result_2') # check predicted_name<-
})

# test model_seq objects
test_that('model objects',{

    M=model()
    expect_warning(model_train(M,DatasetExperiment())) # check default model is to do nothing and throws a warning
    expect_identical(model_predict(M,DatasetExperiment()),M) # check default prediction is nothing

    # some test data
    D=iris_DatasetExperiment()

    # a test model object
    # adds two input values
    test_model=function(...) {
        out = .test_model()
        out = .initialize_struct_class(out,...)
        return(out)
    }
    
    .test_model=setClass('test_model',
                        contains='model',
                        slots=c(
                            params_value_1='numeric',
                            params_value_2='numeric',
                            outputs_result_1='DatasetExperiment',
                            outputs_result_2='DatasetExperiment'
                        ),
                        prototype=list(predicted='result_2',type='test')
    )

    setMethod(f='model_train',
              signature=c('test_model','DatasetExperiment'),
              definition=function(M,D){
                  D$data=D$data+M$value_1
                  M$result_1 = D
                  return(M)
              })

    setMethod(f='model_predict',
              signature=c('test_model','DatasetExperiment'),
              definition=function(M,D){
                  D$data=D$data+M$value_2
                  M$result_2=D
                  return(M)
              })

    # a model sequence
    TM = test_model('value_1'=10,'value_2'=5) + test_model('value_1'=1,'value_2'=2)

    # check return list of models
    expect_identical(models(TM),list(test_model('value_1'=10,'value_2'=5), test_model('value_1'=1,'value_2'=2)))
    # check assign list of models
    TM2 = test_model('value_1'=2,'value_2'=2) + test_model('value_1'=2,'value_2'=2) # different values
    expect_equal({
        models(TM2)=list(test_model(value_1=10,value_2=5),test_model(value_1=1,value_2=2)) # assign list with new values
        TM2[1]$value_1
    },10) # check value is as expected


    # train/predict
    TM = model_train(TM,D)
    TM = model_predict(TM,D)

    expect_equal(TM[1]$value_1,10) # check values assigned correctly
    expect_equal(TM[1]$value_2,5) # check values assigned correctly

    # check model_train()
    expect_identical(matrix(TM[1]$result_1$data),matrix(iris[,1:4]+10)) # value_1 added
    expect_identical(matrix(TM[1]$result_2$data),matrix(iris[,1:4]+5))  # value_2 added
    expect_identical(matrix(TM[2]$result_1$data),matrix(iris[,1:4]+6)) # value_2 added then value_1 added (output from [1] to input of [2])
    expect_identical(matrix(TM[2]$result_2$data),matrix(iris[,1:4]+7)) # value_2 added then value_2 added

    # check can only add models to sequence
    expect_error(TM+DatasetExperiment())

    # check can only insert models by index
    expect_error({TM[1]=DatasetExperiment()})

    # check only models if assigning by list
    expect_error({models(TM)=list(D,D)})

    # check show
    expect_output(show(TM),'A model_seq object containing:') # if contains models
    expect_output(show(model_seq()),'no models') # if no models

    # check add model at end of sequence
    TM=TM+test_model(value_1=50,value_2=50)
    expect_equal(TM[3]$value_1,50)

    # check add model at start of sequence
    TM=test_model(value_1=50,value_2=50)+TM
    expect_equal(TM[1]$value_1,50)
})

