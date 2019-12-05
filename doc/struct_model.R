## -----------------------------------------------------------------------------
set_struct_obj(
  class_name = 'add_two_inputs',
  struct_obj = 'model',
  stato = FALSE,
  params=c(input_1 = 'numeric', input_2 = 'numeric'),
  outputs=c(result = 'numeric'),
  prototype=list(
    input_1 = 0, 
    input_2 = 0,
    name='Add two inputs',
    description='example class that adds two values together',
    predicted='result'
    )
  )

## -----------------------------------------------------------------------------
# show the class definition
show(add_two_inputs)

## -----------------------------------------------------------------------------
set_obj_method(
  class_name = 'add_two_inputs',
  method_name = 'model_apply',
  definition = function(M,D) { # you need to supply D here even if you dont use it
    M$result = M$input_1 + M$input_2
    return(M)                  # remember to always return the input object after modifying it
  }
)

## -----------------------------------------------------------------------------
# create an instance of the model
M = add_two_inputs(input_1 = 3, input_2 = 5)
# use the model
M = model_apply(M,dataset())
# check the result = 8
M$result

## -----------------------------------------------------------------------------
show(add_two_inputs())

## -----------------------------------------------------------------------------
# update the show method
set_obj_show(
  class_name = 'add_two_inputs',
  extra_string = function(x) {
    str= paste('This model is currently set to calculate: ',x$input_1,' + ', x$input_2,sep='')
    return(str)
  }
)


## -----------------------------------------------------------------------------
# call the show method
show(M)

## -----------------------------------------------------------------------------
## example use
# create an instance of your object
M = example_model()
# get/set parameters
M$value_1 = 5
M$value_1 # 5
# train your model with some data
M = model_train(M,iris_dataset())
# apply your model to some test data
M = model_predict(M,iris_dataset())

## -----------------------------------------------------------------------------
model_template=setClass('model_template', # replace model_template with ...
    # ...your new model name
    contains = c('model','stato'),       # stato is optional
    slots=c(                      # define your parameters and outputs here
        'params_value_0'='entity',
        'params_value_1'='entity_stato',
        'params_value_2'='numeric',
        'outputs_result_1'='entity',
        'outputs_result_2'='numeric'
    ),
    prototype = list( # specify default values for your parameters etc
        
        ## These are the default slots available for every struct object
        name='A test model',
        description='An example model object. Training adds value_1 counts to
    a dataset, while prediction adds value_2 counts.',
        type='test',
        
        ## This slot is only required for model_stato objects
        stato_id='OBI:0000011',
        
        ## parameters all start with params_
        # entities can be initialised with populated slots
        params_value_0=entity(name='Value 0',value=0,type='numeric'),
        
        # entity_stato objects can have a stato_id
        params_value_1=entity_stato(value=10,name='Value 1',type='numeric',
            description='An example entity_stato object',
            stato_id='STATO:0000047'),
        
        # params dont have to be entity objects but we dont recommend this.
        params_value_2=20,
        
        # entities can be initialised with populated slots
        outputs_result_1=entity(name='Result 1',type='dataset',
            description='An example entity object',value=dataset()),
        
        # outputs dont have to be entity objects but we dont recommend this.
        outputs_result_2=2
    )
)

## -----------------------------------------------------------------------------
# create a model_train method for your object
setMethod(f='model_train', # dont change this line
    signature=c('model_template','dataset'),  # replace model_template with...
    # ...your new model name
    definition = function(M,D) {              # dont change this line
        # do something here #
        return(M)                             # make sure you return the model
    }
)

## -----------------------------------------------------------------------------
# create a model_predict method for your object
setMethod(f='model_predict',                  # dont change this line
    signature=c('model_template','dataset'),  # replace model_template with...
    # ...your new model name
    definition = function(M,D) {              # dont change this line
        ## do something here ##
        return(M)                             # make sure you return the model 
    }
)

