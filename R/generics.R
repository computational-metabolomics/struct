#####################################
###### parameter class generics #####
#####################################

#' Get/set parameters for an object
#'
#' Gets or sets the object of a parameter e.g. to an entity() object.
#' @param obj a model or iterator object from the *struct* class
#' @param name name of parameter as a string
#' @param value a valid value for the parameter being set
#' @rdname param
#' @export
#' @return
#' \describe{
#' \item{\code{param.obj(M,name)}}{returns the named parameter as an object}
#' \item{\code{param.obj(M,name)<-}}{sets the named parameter of an object}
#' }
#' @examples
#' # get the parameter as an object
#' M = test_model()
#' obj = param.obj(M, 'value_1')
#'
#' # set a parameter as an object
#' param.obj(M, 'value_1') = entity(value = 15)
#'
setGeneric("param.obj",function(obj,name)standardGeneric("param.obj"))

#' @rdname param
#' @export
#' @examples
#'
setGeneric("param.obj<-",function(obj,name,value)standardGeneric("param.obj<-"))

#' Verify parameter
#'
#' Verify that the name of a paramater is valid for an object
#' @param obj a model or iterator object from the *struct* class
#' @param name name of parameter as a string
#' @return TRUE if parameter name is valid, FALSE if not
#' @export
#' @examples
#' M = test_model()
#' is.param(M,'value_1') # TRUE
#' is.param(M,'alpha')   # FALSE
#'
setGeneric("is.param",function(obj,name)standardGeneric("is.param"))

#' parameter identifiers
#'
#' return a list of valid parameter ids for an object
#' @param obj a model or iterator object from the *struct* class
#' @return list of parameter ids
#' @export
#' @examples
#' M = test_model()
#' param.ids(M)
#'
setGeneric("param.ids",function(obj)standardGeneric("param.ids"))

#' parameter name
#'
#' return a the name for a paramater, if available
#' @param obj a model or iterator object from the *struct* class
#' @param name id of parameter
#' @return name of parameter
#' @export
#' @examples
#' M = test_model()
#' param.name(M,'value_1')
#'
setGeneric("param.name",function(obj,name)standardGeneric("param.name"))

#' parameter list
#'
#' get/set a named list of parameters and thier current value for an object
#' @param obj a model or iterator object from the *struct* class
#' @param value a named list
#' @return list of parameter names
#' @rdname param.list
#' @export
#' @examples
#' M = test_model()
#' L = param.list(M)
#'
setGeneric("param.list",function(obj)standardGeneric("param.list"))

#' @rdname param.list
#' @export
#' @return struct object
#' @examples
#' M = test_model()
#' param.list(M)=list('value_1' = 15,'value_2' = 20)
#'
setGeneric("param.list<-",function(obj,value)standardGeneric("param.list<-"))

#' parameter values
#'
#' get/set the values for a parameter.
#' @param obj a model or iterator object from the *struct* class
#' @param name of a parameter
#' @param value value
#' @return value of parameter
#' @rdname param.value
#' @export
#' @examples
#' M = test_model()
#' param.value(M,'value_1')
#'
setGeneric("param.value",function(obj,name)standardGeneric("param.value"))

#' @rdname param.value
#' @export
#' @return struct object
#' @examples
#' M = test_model()
#' param.value(M,'value_1') = 0.95
#'
setGeneric("param.value<-",
    function(obj,name,value)standardGeneric("param.value<-"))

#####################################
###### output class generics #####
#####################################

#' output identifiers
#'
#' return a list of valid output ids for an object
#' @param obj a model or iterator object from the *struct* class
#' @return list of output ids
#' @export
#' @examples
#' \dontrun{
#'
#' output.ids(M)
#' }
setGeneric("output.ids",function(obj)standardGeneric("output.ids"))

#' output values
#'
#' get/set the values for an output.
#' @param obj a model or iterator object from the *struct* class
#' @param name name of an output
#' @param value value
#' @return value of output
#' @rdname output.value
#' @export
#' @examples
#' \dontrun{
#'
#' output.value(M,'value_1')
#' }
setGeneric("output.value",function(obj,name)standardGeneric("output.value"))

#' @rdname output.value
#' @export
#' @return object
#' @examples
#' \dontrun{
#'
#' output.value(M,'value_1') = 10
#' }
setGeneric("output.value<-",
    function(obj,name,value)standardGeneric("output.value<-"))

#' get/set outputs for an object
#'
#' @param obj a model or iterator object from the *struct* class
#' @param name name of output as a string
#' @rdname outputs
#' @export
#' @return object
#' @examples
#' \dontrun{
#'
#' x = output.obj(M,'value_1')
#' }
setGeneric("output.obj",
    function(obj,name)standardGeneric("output.obj"))

#' @export
#' @param value a valid value for the parameter being set
#' @rdname outputs
#' @return object
#' @examples
#' \dontrun{
#'
#' output.value(M,'value_1') = 10
#' }
setGeneric("output.obj<-",
    function(obj,name,value)standardGeneric("output.obj<-"))

#' verify output name
#'
#' verify that the name of an output is valid for an object
#' @param obj a model or iterator object from the *struct* class
#' @param name name of the output as a string
#' @return TRUE if output name is valid, FALSE if not
#' @export
#' @examples
#' \dontrun{
#'
#' is.input(M,'value_1')
#' }
setGeneric("is.output",function(obj,name)standardGeneric("is.output"))

#' output names
#'
#' return a list of valid parameter names for an object
#' @param obj a model or iterator object from the *struct* class
#' @param name id of output
#' @return list of parameter names
#' @export
#' @examples
#' \dontrun{
#'
#' output.name(M,'value_1')
#' }
setGeneric("output.name",function(obj,name)standardGeneric("output.name"))

#' output list
#'
#' get a named list of outputs and thier current value for an object
#' @param obj a model or iterator object from the *struct* class
#' @param value a named list of outputs for an object
#' @return list of output names
#' @rdname output.list
#' @export
#' @return object
#' @examples
#' \dontrun{
#'
#' output.list(M)
#' }
setGeneric("output.list",function(obj)standardGeneric("output.list"))

#' @rdname output.list
#' @export
#' @return list
#' @examples
#' \dontrun{
#'
#' output.list(M) = liist('value_1'=10)
#' }
setGeneric("output.list<-",function(obj,value)standardGeneric("output.list<-"))

#####################################
###### chart class generics #####
#####################################

#' chart names
#'
#' return a list of valid charts for a struct object
#' @param obj a object from the *struct* package
#' @param ... optional inputs
#' @export
#' @return list of chart names
#' @examples
#' \dontrun{
#'
#' D = dataset()
#' chart.names(D)
#' }
setGeneric("chart.names",function(obj,...)standardGeneric("chart.names"))

#' plot a chart for an object
#'
#' @param obj a chart object
#' @param dobj a struct object
#' @param ... optional inputs
#' @rdname chart.plot
#' @return plot
#' @examples
#' \dontrun{
#'
#' C=chart()
#' chart.plot(C)
#' p=chart.plot(C) # can return e.g. ggplot object
#' }
#' @export
setGeneric("chart.plot",function(obj, dobj, ...)standardGeneric("chart.plot"))

#####################################
###### struct class generics #####
#####################################

#' get/set type for an object
#'
#' @param obj an object from the \pkg{struct} class
#' @param value a valid type string
#' @rdname type
#' @export
setGeneric("type",function(obj)standardGeneric("type"))

#' @export
#' @rdname type
setGeneric("type<-",function(obj,value)standardGeneric("type<-"))

#' get/set name for an object
#'
#' @param obj an object from the \pkg{struct} class
#' @param value a valid string
#' @rdname name
#' @export
setGeneric("name",function(obj)standardGeneric("name"))

#' @export
#' @rdname name
setGeneric("name<-",function(obj,value)standardGeneric("name<-"))

#' get/set description for an object
#'
#' @param obj an object from the \pkg{struct} class
#' @param value a valid string
#' @rdname desc

#' @export
setGeneric("description",function(obj)standardGeneric("description"))

#' @export
#' @rdname desc
setGeneric("description<-",function(obj,value)standardGeneric("description<-"))

#################################
###### method class generics ####
#################################

#' Apply a method
#'
#' Applies a method to the input dataset
#' @param M a model object
#' @param D a dataset object
#' @rdname method.apply
#' @export
setGeneric("method.apply",function(M,D)standardGeneric("method.apply"))

#################################
###### model class generics #####
#################################

#' Train a model
#'
#' Trains a model using the input dataset
#' @param M a model object
#' @param D a dataset object
#' @rdname train
#' @export
setGeneric("model.train",function(M,D)standardGeneric("model.train"))

#' Apply a (trained) model to a dataset D to get a prediction
#'
#' Apply a model using the input dataset. Requires that the model is trained
#' first.
#' @param M a model object
#' @param D a dataset object

#' @rdname predict
#' @export
setGeneric("model.predict",function(M,D)standardGeneric("model.predict"))

#' get/set output name as prediction output for a model
#'
#' get/set the prediction output for a model. This determines which outputs from
#' this model are supplied as inputs to the next model when used in a model.seq
#' @param M a model object
#' @param value name of an output for this model

#' @rdname predicted.name
#' @export
setGeneric("predicted.name",function(M)standardGeneric("predicted.name"))

#' @export
#' @inheritParams predicted.name
#' @rdname predicted.name
setGeneric("predicted.name<-",
    function(M,value)standardGeneric("predicted.name<-"))

#' prediction output for a model
#'
#' returns the prediction output for a model. This is supplied as input to the
#' next model when used in a model.seq
#' @param M a model object
#' @rdname predicted
#' @export
setGeneric("predicted",function(M)standardGeneric("predicted"))

######################################
###### model.seq class generics #####
######################################

#' Get/set models of a model.seq
#'
#' Returns the list of models in a model.seq object
#' @param ML a model.seq object
#' @param value a list containing only model objects
#' @rdname models
#' @export
setGeneric("models",function(ML)standardGeneric("models"))

#' @export
#' @rdname models
setGeneric("models<-",function(ML,value)standardGeneric("models<-"))

###################################
###### dataset class generics #####
###################################

#' get/set data for a dataset object
#'
#' @param obj a type object from the *struct* class
#' @param value a data.frame of raw data
#' @rdname data
#' @export
setGeneric("dataset.data",
    function(obj)standardGeneric("dataset.data"))

#' @export
#' @rdname data
setGeneric("dataset.data<-",
    function(obj,value)standardGeneric("dataset.data<-"))

#' get/set sample meta data for a dataset object
#'
#' @param obj a type object from the *struct* class
#' @param value a data.frame of meta data
#' @rdname sample_meta

#' @export
setGeneric("dataset.sample_meta",
    function(obj)standardGeneric("dataset.sample_meta"))

#' @export
#' @rdname sample_meta
setGeneric("dataset.sample_meta<-",
    function(obj,value)standardGeneric("dataset.sample_meta<-"))

#' get/set variable meta data for a dataset object
#'
#' @param obj a type object from the *struct* class
#' @param value a data.frame of meta data
#' @rdname variable_meta

#' @export
setGeneric("dataset.variable_meta",
    function(obj)standardGeneric("dataset.variable_meta"))

#' @export
#' @rdname variable_meta
setGeneric("dataset.variable_meta<-",
    function(obj,value)standardGeneric("dataset.variable_meta<-"))

####################################
###### iterator class generics #####
####################################

#' run an iterator object
#'
#' @param I an iterator object
#' @param D a dataset object
#' @param MET a metric object
#' @rdname iterator
#' @export
setGeneric("run",function(I,D,MET)standardGeneric("run"))

#' evaluate an iterator object
#'
#' @rdname iterator
#' @export
setGeneric("evaluate",function(I,MET)standardGeneric("evaluate"))

#' get/set output name as prediction output for a model
#'
#' get/set the prediction output for a model. This determines which outputs
#' from this model are supplied as inputs to the next model when used in
#' a model.seq
#' @param M an iterator object
#' @param I an iterator object
#' @param value name of an output for iterator M
#' @rdname result.name
#' @export
setGeneric("result.name",function(M)standardGeneric("result.name"))

#' @export
#' @rdname result.name
setGeneric("result.name<-",function(I,value)standardGeneric("result.name<-"))

#' results output for an iterator
#'
#' returns the results of an iterator. This is used to control model flow.
#' @param M an iterator object
#' @rdname result
#' @export
setGeneric("result",function(M)standardGeneric("result"))

####################################
###### metric class generics #####
####################################

#' calculate a metric
#'
#' @param obj a metric object
#' @param value value
#' @param ... additional inputs depending on object
#' @rdname metric
#' @export
setGeneric("calculate",function(obj,...)standardGeneric("calculate"))

#' get the value for a metric
#'
#' @return value the calculated value of a metric
#' @rdname metric
#' @export
setGeneric("value",function(obj)standardGeneric("value"))

#' set the valye for a metric
#'
#' @export
#' @param obj metric object
#' @rdname metric
setGeneric("value<-",function(obj,value)standardGeneric("value<-"))

####################################
###### stato class generics #####
####################################

#' get the stato.id for an object
#'
#' @return id the stato id
#' @param obj stato_class object
#' @param outpath folder path
#' @param filename file name
#' @param outformat output format ("pdf" or "html")
#' @rdname stato
#' @export
setGeneric("stato.id",function(obj)standardGeneric("stato.id"))

#' get the stato name for an object
#'
#' @return name the stato name
#' @rdname stato
#' @export
setGeneric("stato.name",function(obj)standardGeneric("stato.name"))

#' get the stato name for an object
#'
#' @return def the stato description
#' @rdname stato
#' @export
setGeneric("stato.definition",function(obj)standardGeneric("stato.definition"))

#' display a stato summary of the object
#'
#' @rdname stato
#' @export
setGeneric("stato.summary",function(obj)standardGeneric("stato.summary"))

#' get a pdf of a stato object
#'
#' @rdname stato
#' @export
setGeneric("stato.pdf",
    function(obj,outpath,filename,outformat)standardGeneric("stato.pdf"))

######################################
###### method.seq class generics #####
######################################

#' Get/set methods of a method.seq
#'
#' Returns the list of method in a method.seq object
#' @param ML a method.seq object
#' @param value a list containing only method objects
#' @rdname methods
#' @export
setGeneric("method.steps",function(ML)standardGeneric("method.steps"))

#' @export
#' @rdname methods
setGeneric("method.steps<-",function(ML,value)standardGeneric("method.steps<-"))
