#####################################
###### parameter class generics #####
#####################################

#' Parameter objects
#'
#' Gets or sets the object of a parameter e.g. to an entity() object.
#' @param obj a model or iterator object from the *struct* class
#' @param name name of parameter as a string
#' @param value a valid value for the parameter being set
#' @rdname param.obj
#' @export
#' @return
#' \describe{
#' \item{\code{param.obj(M,name)}}{returns the named parameter as an object}
#' \item{\code{param.obj(M,name)<-}}{sets the named parameter of an object}
#' }
#' @examples
#' # get the parameter as an object
#' M = example_model()
#' obj = param.obj(M, 'value_0')
#'
#' # set a parameter as an object
#' param.obj(M, 'value_0') = entity(value = 15)
#'
setGeneric("param.obj",function(obj,name)standardGeneric("param.obj"))

#' @rdname param.obj
#' @export
setGeneric("param.obj<-",function(obj,name,value)standardGeneric("param.obj<-"))

#' Verify parameter
#'
#' Verify that the name of a paramater is valid for an object
#' @param obj a model or iterator object from the *struct* class
#' @param name name of parameter as a string
#' @return TRUE if parameter name is valid, FALSE if not
#' @export
#' @examples
#' M = example_model()
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
#' M = example_model()
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
#' M = example_model()
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
#' M = example_model()
#' L = param.list(M)
#'
setGeneric("param.list",function(obj)standardGeneric("param.list"))

#' @rdname param.list
#' @export
#' @return struct object
#' @examples
#' M = example_model()
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
#' M = example_model()
#' param.value(M,'value_1')
#'
setGeneric("param.value",function(obj,name)standardGeneric("param.value"))

#' @rdname param.value
#' @export
#' @return struct object
#' @examples
#' M = example_model()
#' param.value(M,'value_1') = 0.95
#'
setGeneric("param.value<-",
    function(obj,name,value)standardGeneric("param.value<-"))

#####################################
###### output class generics #####
#####################################

#' Output identifiers
#'
#' return a list of valid output ids for an object
#' @param obj a model or iterator object from the *struct* class
#' @return list of output ids
#' @export
#' @examples
#' M = example_model()
#' output.ids(M)
#'
setGeneric("output.ids",function(obj)standardGeneric("output.ids"))

#' output values
#'
#' get/set the values for an output.
#' @param obj a model or iterator object from the *struct* class
#' @param name of an output
#' @param value value
#' @return value of output
#' @rdname output.value
#' @export
#' @examples
#' M = example_model()
#' output.value(M,'result_1')
#'
setGeneric("output.value",function(obj,name)standardGeneric("output.value"))

#' @rdname output.value
#' @export
#' @return struct object
#' @examples
#' M = example_model()
#' output.value(M,'result_1') = 0.95
#'
setGeneric("output.value<-",
    function(obj,name,value)standardGeneric("output.value<-"))

#' Output objects
#'
#' Gets or sets the object of an output e.g. to an entity() object.
#' @param obj a model or iterator object from the *struct* class
#' @param name name of output as a string
#' @param value a valid value for the output being set
#' @rdname output.obj
#' @export
#' @return
#' \describe{
#' \item{\code{output.obj(M,name)}}{returns the named output as an object}
#' \item{\code{output.obj(M,name)<-}}{sets the named output of an object}
#' }
#' @examples
#' # get the output as an object
#' M = example_model()
#' obj = output.obj(M, 'result_1')
#'
#' # set a output as an object
#' output.obj(M, 'result_1') = entity(value = 15)
#'
setGeneric("output.obj",
    function(obj,name)standardGeneric("output.obj"))

#' @export
#' @rdname output.obj
setGeneric("output.obj<-",
    function(obj,name,value)standardGeneric("output.obj<-"))

#' Verify output
#'
#' Verify that the name of a output is valid for an object
#' @param obj a model or iterator object from the *struct* class
#' @param name name of output as a string
#' @return TRUE if output name is valid, FALSE if not
#' @export
#' @examples
#' M = example_model()
#' is.output(M,'result_1') # TRUE
#' is.output(M,'result_0')   # FALSE
#'
setGeneric("is.output",function(obj,name)standardGeneric("is.output"))

#' output name
#'
#' return a the name for a output, if available
#' @param obj a model or iterator object from the *struct* class
#' @param name id of output
#' @return name of output
#' @export
#' @examples
#' M = example_model()
#' output.name(M,'result_1')
#'
setGeneric("output.name",function(obj,name)standardGeneric("output.name"))

#' output list
#'
#' get/set a named list of outputs and their current value for an object
#' @param obj a model or iterator object from the *struct* class
#' @param value a named list
#' @return list of output names
#' @rdname output.list
#' @export
#' @examples
#' M = example_model()
#' L = output.list(M)
#'
setGeneric("output.list",function(obj)standardGeneric("output.list"))

#' @rdname output.list
#' @export
#' @return struct object
#' @examples
#' M = example_model()
#' output.list(M)=list('result_1' = dataset(),'result_2' = dataset())
#'
setGeneric("output.list<-",function(obj,value)standardGeneric("output.list<-"))

#####################################
###### chart class generics #####
#####################################

#' chart names
#'
#' Returns a list of valid charts for a struct object
#'
#' The chart.names method searches chart objects that specify the input object
#' type as an input.
#'
#' Optional input \code{ret = 'char'} is the default and will return a character
#' vector of object names. \code{ret = 'obj'} will return a list containing the
#' named chart objects.
#' @param obj a object from the *struct* package
#' @param ... optional inputs (see details)
#' @export
#' @return list of chart names
#' @examples
#' M = example_model()
#' chart.names(M) # 'example_chart'
#' chart.names(M,'char') # as above
#' chart.names(M,'obj') # returns a list of chart objects
#'
setGeneric("chart.names",function(obj,...)standardGeneric("chart.names"))

#' chart.plot
#'
#' Plots a chart object
#' @param obj a chart object
#' @param dobj a struct object
#' @param ... optional inputs
#' @rdname chart.plot
#' @return a plot object
#' @examples
#' C = example_chart()
#' chart.plot(C)
#'
#' @export
setGeneric("chart.plot",function(obj, dobj, ...)standardGeneric("chart.plot"))

#####################################
###### struct class generics #####
#####################################

#' Object Type
#'
#' Get/set the type of a object. Can be used to specify e.g. where a model
#' is for classification or for regression, or a dataset is single or multi
#' block for example.
#' @param obj an object from the \pkg{struct} class
#' @param value a valid type string
#' @rdname type
#' @export
#' @return
#' \describe{
#' \item{\code{type(obj)}}{returns the type of an object as a string}
#' \item{\code{type(obj)<-}}{sets the type of an object and returns the modified
#' object}
#' }
#' @examples
#' M = example_model()
#' type(M)
#' type(M) = 'example'
#'
setGeneric("type",function(obj)standardGeneric("type"))

#' @export
#' @rdname type
setGeneric("type<-",function(obj,value)standardGeneric("type<-"))

#' Object Name
#'
#' Get/set the name of an object. This is the long name for an object, not the
#' name of the class object.
#' @param obj an object from the \pkg{struct} class
#' @param value a valid string
#' @rdname name
#' @return
#' \describe{
#' \item{\code{name(obj)}}{returns the name of an object as a string}
#' \item{\code{name(obj)<-}}{sets the name of an object and returns the modified
#' object}
#' }
#' @export
#' @examples
#' M = example_model()
#' name(M)
#' name(M) = 'example object'
setGeneric("name",function(obj)standardGeneric("name"))

#' @export
#' @rdname name
setGeneric("name<-",function(obj,value)standardGeneric("name<-"))

#' Object Description
#'
#' Get/set the description of an object.
#' @param obj an object from the \pkg{struct} class
#' @param value a valid string
#' @rdname desc
#' @return
#' \describe{
#' \item{\code{description(obj)}}{returns the description of an object as a
#' string}
#' \item{\code{description(obj)<-}}{sets the description of an object and
#' returns the modified object}
#' }
#' @export
#' @examples
#' M = example_model()
#' description(M)
#' description(M) = 'this is an example'
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
#' @param M a method object
#' @param D another object used by the first
#' @return Returns a modified method object
#' @rdname method.apply
#' @export
#' @examples
#' M = example_method()
#' M = method.apply(M,iris_dataset())
#'
setGeneric("method.apply",function(M,D)standardGeneric("method.apply"))

#################################
###### model class generics #####
#################################

#' Train a model
#'
#' Trains a model using the input dataset
#' @param M a model object
#' @param D a dataset object
#' @return Returns a modified model object
#' @rdname train
#' @export
#' @examples
#' M = example_model()
#' M = model.train(M,iris_dataset())
#'
setGeneric("model.train",function(M,D)standardGeneric("model.train"))

#' Model prediction
#'
#' Apply a model using the input dataset. Assumes the model is trained
#' first.
#' @param M a model object
#' @param D a dataset object
#' @return Returns a modified model object
#' @rdname predict
#' @export
#' @examples
#' M = example_model()
#' M = model.predict(M,iris_dataset())
#'
setGeneric("model.predict",function(M,D)standardGeneric("model.predict"))

#' Reverse preprocessing
#'
#' Reverse the effect of a preprocessing step on a dataset.
#' @param M a model object
#' @param D a dataset object
#' @return Returns a modified dataset object
#' @rdname model.reverse
#' @export
#' @examples
#' M = example_model()
#' D = model.reverse(M,iris_dataset())
#'
setGeneric("model.reverse",function(M,D)standardGeneric("model.reverse"))

#' Predicted output name
#'
#' get/set the prediction output for a model. This determines which outputs from
#' this model are supplied as inputs to the next model when used in a model.seq
#' @param M a model object
#' @param value name of an output for this model
#' @return
#' \describe{
#' \item{\code{predicted.name}}{returns the name of the predicted output}
#' \item{\code{predicted.name<-}}{sets the name of the predicted output and
#' returns the modified object}
#' }
#' @rdname predicted.name
#' @export
#' @examples
#' M = example_model()
#' predicted.name(M)
#' predicted.name(M) = 'result_2'
#'
setGeneric("predicted.name",function(M)standardGeneric("predicted.name"))

#' @export
#' @rdname predicted.name
setGeneric("predicted.name<-",
    function(M,value)standardGeneric("predicted.name<-"))

#' Prediction output
#'
#' returns the prediction output for a model. This is supplied as input to the
#' next model when used in a model.seq
#' @param M a model object
#' @return The value returned varies depending on the output.
#' @rdname predicted
#' @export
#' @examples
#' M = example_model()
#' M = model.train(M, iris_dataset())
#' M = model.predict(M, iris_dataset())
#' predicted(M)
#'
setGeneric("predicted",function(M)standardGeneric("predicted"))

######################################
###### model.seq class generics #####
######################################

#' Get/set models of a model.seq
#'
#' Returns the list of models in a model.seq object
#' @param ML a model.seq object
#' @param value a list containing only model objects
#' @return
#' \describe{
#' \item{\code{models(ML)}}{returns a list of models in the model sequence}
#' \item{\code{models(ML)<-}}{sets the list of models in the model sequence}
#' }
#' @rdname models
#' @export
#' @examples
#' # Create a model sequence
#' ML = model.seq()
#' models(ML) = list(example_model(), example_model())
#' models(ML)
#'
setGeneric("models",function(ML)standardGeneric("models"))

#' @export
#' @rdname models
setGeneric("models<-",function(ML,value)standardGeneric("models<-"))

###################################
###### dataset class generics #####
###################################

#' Dataset object data
#'
#' @param obj a type object from the *struct* class
#' @param value a data.frame of raw data
#' @rdname data
#' @return
#' \describe{
#' \item{\code{dataset.data(obj)}}{returns a data.frame of raw data}
#' \item{\code{dataset.data(obj)<-}}{sets the raw data of a dataset object}
#' }
#' @export
#' @examples
#' # iris dataset
#' D = iris_dataset()
#' # original raw data
#' X = dataset.data(D)
#' # modify the raw dataset
#' dataset.data(D) = X + 1
#'
setGeneric("dataset.data",
    function(obj)standardGeneric("dataset.data"))

#' @export
#' @rdname data
setGeneric("dataset.data<-",
    function(obj,value)standardGeneric("dataset.data<-"))

#' Sample meta data
#'
#' get/set sample meta data for a dataset object
#' @param obj a type object from the *struct* class
#' @param value a data.frame of meta data
#' @rdname sample_meta
#' @return
#' \describe{
#' \item{\code{dataset.sample_meta(obj)}}{returns a data.frame of meta data}
#' \item{\code{dataset.sample_meta(obj)<-}}{sets the meta data of a dataset
#' object}
#' }
#' @examples
#' # iris dataset
#' D = iris_dataset()
#' X = dataset.sample_meta(D)
#' dataset.sample_meta(D) = X
#' @export
setGeneric("dataset.sample_meta",
    function(obj)standardGeneric("dataset.sample_meta"))

#' @export
#' @rdname sample_meta
setGeneric("dataset.sample_meta<-",
    function(obj,value)standardGeneric("dataset.sample_meta<-"))

#' Variable meta data
#'
#' get/set variable meta data for a dataset object
#' @param obj a type object from the *struct* class
#' @param value a data.frame of meta data
#' @rdname variable_meta
#' @return
#' \describe{
#' \item{\code{dataset.variable_meta(obj)}}{returns a data.frame of meta data}
#' \item{\code{dataset.variable_meta(obj)<-}}{sets the meta data of a dataset
#' object}
#' }
#' @export
#' @examples
#' # iris dataset
#' D = iris_dataset()
#' X = dataset.variable_meta(D)
#' dataset.variable_meta(D) = X
#'
setGeneric("dataset.variable_meta",
    function(obj)standardGeneric("dataset.variable_meta"))

#' @export
#' @rdname variable_meta
setGeneric("dataset.variable_meta<-",
    function(obj,value)standardGeneric("dataset.variable_meta<-"))

####################################
###### iterator class generics #####
####################################

#' Run iterator
#'
#' Runs an iterator, applying the chosen model multiple times.
#'
#' Running an iterator will apply the iterator a number of times to a dataset.
#' For example, in cross-validation the same model is applied multiple times to
#' the same data, splitting it into training and test sets. The input metric
#' object can be calculated and collected for each iteration as an output.
#' @param I an iterator object
#' @param D a dataset object
#' @param MET a metric object
#' @rdname iterator
#' @export
#' @return Modified iterator object
#' @examples
#' D = iris_dataset() # get some data
#' MET = metric()  # use a metric
#' I = example_iterator() # initialise iterator
#' models(I) = example_model() # set the model
#' I = run(I,D,MET) # run
#'
setGeneric("run",function(I,D,MET)standardGeneric("run"))

#' Evaluate an iterator
#'
#' Evaluates an iterator by e.g. averaging over all iterations. May be
#' deprecated in a future release as \code{evaluate} is applied by \code{run}
#' anyway.
#' @rdname iterator
#' @export
#' @return Modified iterator object
#' @examples
#' D = iris_dataset() # get some data
#' MET = metric()  # use a metric
#' I = example_iterator() # initialise iterator
#' models(I) = example_model() # set the model
#' I = run(I,D,MET) # run
#' I = evaluate(I,MET) # evaluate
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
#' @return
#' \describe{
#' \item{\code{result.name(M)}}{returns the name of the output for this iterator
#' (equivalent to \code{predicted} for model objects)}
#' \item{\code{result.name(I)<-}}{sets the default output for an iterator}
#' }
#' @examples
#' I = example_iterator() # initialise iterator
#' result.name(I)
#' result.name(I) = 'result_1'
#'
setGeneric("result.name",function(M)standardGeneric("result.name"))

#' @export
#' @rdname result.name
setGeneric("result.name<-",function(I,value)standardGeneric("result.name<-"))

#' Iterator result
#'
#' Returns the results of an iterator. This is used to control model flow in a
#' similar way to \code{predict} for model and model.seq objects.
#' @param M an iterator object
#' @rdname result
#' @export
#' @return the returned output varies with the algorithm implemented
#' @examples
#' D = iris_dataset() # get some data
#' MET = metric()  # use a metric
#' I = example_iterator() # initialise iterator
#' models(I) = example_model() # set the model
#' I = run(I,D,MET) # run
#' result(I)
#'
setGeneric("result",function(M)standardGeneric("result"))

####################################
###### metric class generics #####
####################################

#' Calculate metric
#'
#' @param obj a metric object
#' @param value value
#' @param ... additional inputs depending on object
#' @rdname metric
#' @export
#' @examples
#' MET = metric()
#' calculate(MET)
#'
setGeneric("calculate",function(obj, ...)standardGeneric("calculate"))

#' get the value for a metric
#'
#' @return value the calculated value of a metric
#' @rdname metric
#' @export
setGeneric("value",function(obj)standardGeneric("value"))

#' set the value for a metric
#'
#' @export
#' @rdname metric
setGeneric("value<-",function(obj,value)standardGeneric("value<-"))

#' get the max value vector length for an entity
#'
#' @return max value vector length for an entity
#' @rdname entity
#' @export
setGeneric("max_length",function(obj)standardGeneric("max_length"))

#' set the value for a metric
#'
#' @export
#' @rdname metric
setGeneric("max_length<-",function(obj,value)standardGeneric("max_length<-"))

####################################
###### stato class generics #####
####################################

#' get the stato.id for an object
#'
#' @return id the stato id
#' @param obj stato_class object
#' @rdname stato
#' @export
#' @examples
#' M = example_model()
#' stato.id(M)
#' stato.name(M)
#' stato.definition(M)
#' stato.summary(M)
#'
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

######################################
###### method.seq class generics #####
######################################

#' Get/set methods of a method.seq
#'
#' Returns the list of method in a method.seq object
#' @param ML a method.seq object
#' @param value a list containing only method objects
#' @rdname methods
#' @return
#' \describe{
#' \item{\code{method.steps(ML)}}{returns a list of methods}
#' \item{\code{method.steps(ML)<-}}{sets the list of methods for a sequence}
#' }
#' @export
#' @examples
#' ML = example_method() + example_method()
#' method.steps(ML)
#' method.steps(ML) = list(example_method(),example_method())
#'
setGeneric("method.steps",function(ML)standardGeneric("method.steps"))

#' @export
#' @rdname methods
setGeneric("method.steps<-",function(ML,value)standardGeneric("method.steps<-"))
