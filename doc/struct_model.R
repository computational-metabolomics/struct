## ------------------------------------------------------------------------
model_template=setClass('model_template', # replace model_template with ...
                                          # ...your new model name
    contains = c('model','stato'),       # stato is optional
    slots=c(                      # define your parameters and outputs here
        'params.value_0'='entity',
        'params.value_1'='entity.stato',
        'params.value_2'='numeric',
        'outputs.result_1'='entity',
        'outputs.result_2'='numeric'
    ),
    prototype = list( # specify default values for your parameters etc
        
        ## These are the default slots available for every struct object
        name='A test model',
        description='An example model object. Training adds value_1 counts to
        a dataset, while prediction adds value_2 counts.',
        type='test',
        
        ## This slot is only required for model.stato objects
        stato.id='OBI:0000011',
        
        ## parameters all start with params.
        # entities can be initialised with populated slots
        params.value_0=entity(name='Value 0',value=0,type='numeric'),
        
        # entity.stato objects can have a stato.id
        params.value_1=entity.stato(value=10,name='Value 1',type='numeric',
            description='An example entity.stato object',
            stato.id='STATO:0000047'),
        
        # params dont have to be entity objects but we dont recommend this.
        params.value_2=20,
        
        # entities can be initialised with populated slots
        outputs.result_1=entity(name='Result 1',type='dataset',
            description='An example entity object'),
        
        # outputs dont have to be entity objects but we dont recommend this.
        outputs.result_2=2
    )
)

# create a model.train method for your object
setMethod(f='model.train', # dont change this line
    signature=c('model_template','dataset'),  # replace model_template with...
                                              # ...your new model name
    definition = function(M,D) {              # dont change this line
        # do something here #
        return(M)                             # make sure you return the model
    }
)

# create a model.predict method for your object
setMethod(f='model.predict',                  # dont change this line
    signature=c('model_template','dataset'),  # replace model_template with...
                                              # ...your new model name
    definition = function(M,D) {              # dont change this line
        ## do something here ##
        return(M)                             # make sure you return the model 
    }
)

