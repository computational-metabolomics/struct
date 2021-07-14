
#####################################
###### parameter class generics #####
#####################################

#' Parameter objects
#'
#' Gets or sets the object of a parameter e.g. to an entity() object.
#' @param obj An object derived from struct_class
#' @param name Name of parameter
#' @param value A valid value for the parameter being set
#' @return
#' \describe{
#' \item{\code{param_obj(M,name)}}{Returns the named parameter as an object}
#' \item{\code{param_obj(M,name)<-}}{Sets the named parameter of an object}
#' }
#' @examples
#' # get the parameter as an object
#' M = example_model()
#' obj = param_obj(M, 'value_0')
#'
#' # set a parameter as an object
#' param_obj(M, 'value_0') = entity(value = 15,type = 'numeric',name='value_0')
#' @export
#' @rdname param_obj
setGeneric("param_obj",function(obj,name)standardGeneric("param_obj"))

#' @rdname param_obj 
#' @export
setGeneric("param_obj<-",function(obj,name,value)standardGeneric("param_obj<-"))

#' Verify parameter
#'
#' Verify that the input name is a valid input parameter for an object
#' @param obj  An object derived from struct_class
#' @param name  Name of parameter
#' @return TRUE if parameter name is valid, FALSE if not
#' @examples
#' M = example_model()
#' is_param(M,'value_1') # TRUE
#' is_param(M,'alpha')   # FALSE
#' @export
#' @rdname is_param
setGeneric("is_param",function(obj,name)standardGeneric("is_param"))

#' Parameter identifiers
#'
#' return a list of valid parameter ids for an object
#' @param obj An object derived from struct_class
#' @return list of parameter ids
#' @examples
#' M = example_model()
#' param_ids(M)
#' @export
#' @rdname param_ids
setGeneric("param_ids",function(obj)standardGeneric("param_ids"))

#' Parameter name
#'
#' Returns the name for a parameter, if available
#' @param obj An object derived from struct_class
#' @param name Name of parameter
#' @return name of parameter
#' @examples
#' M = example_model()
#' param_name(M,'value_1')
#' @export
#' @rdname param_name
setGeneric("param_name",function(obj,name)standardGeneric("param_name"))

#' Parameter list
#'
#' get/set a named list of parameters and thier current value for an object
#' @param obj An object derived from struct_class
#' @param value A named list of parameters and corresponding values
#' @return A named list of parameters names and corresponding values
#' @examples
#' M = example_model()
#' L = param_list(M)
#' 
#' M = example_model()
#' param_list(M) = list('value_1' = 15,'value_2' = 20)
#' @export
#' @rdname param_list
setGeneric("param_list",function(obj)standardGeneric("param_list"))

#' @rdname param_list 
#' @export
setGeneric("param_list<-",function(obj,value)standardGeneric("param_list<-"))

#' Parameter values
#'
#' get/set the values for a parameter.
#' @param obj A model or iterator object derived from structclass
#' @param name Name of parameter
#' @param value A valid value for the parameter being set
#' @return Value of parameter
#' @examples
#' M = example_model()
#' param_value(M,'value_1')
#' 
#' M = example_model()
#' param_value(M,'value_1') = 0.95
#' 
#' @export
#' @rdname param_value
setGeneric("param_value",function(obj,name)standardGeneric("param_value"))

#' @rdname param_value 
#' @export
setGeneric("param_value<-",
    function(obj,name,value)standardGeneric("param_value<-"))

#####################################
###### output class generics #####
#####################################

#' Output identifiers
#'
#' return a list of valid output ids for an object
#' @param obj A model or iterator object derived from the *struct* class
#' @return list of output ids
#' @examples
#' M = example_model()
#' output_ids(M)
#' @export
setGeneric("output_ids",function(obj)standardGeneric("output_ids"))

#' output values
#'
#' get/set the values for an output_
#' @param obj A model or iterator object derived from the *struct* class
#' @param name Name of output
#' @param value A valid value for the output being set
#' @return Value of output
#' @rdname output_value
#' @examples
#' M = example_model()
#' output_value(M,'result_1')
#' @export
setGeneric("output_value",function(obj,name)standardGeneric("output_value"))

#' @rdname output_value
#' @return struct object
#' @examples
#' M = example_model()
#' output_value(M,'result_1') = DatasetExperiment()
#' @export
setGeneric("output_value<-",
    function(obj,name,value)standardGeneric("output_value<-"))

#' Output objects
#'
#' Gets or sets the object of an output e.g. to an entity() object.
#' @param obj A model or iterator object derived from the *struct* class
#' @param name Name of output
#' @param value A valid value for the output being set
#' @rdname output_obj
#' @return
#' \describe{
#' \item{\code{output_obj(M,name)}}{returns the named output as an object}
#' \item{\code{output_obj(M,name)<-}}{sets the named output of an object}
#' }
#' @examples
#' # get the output as an object
#' M = example_model()
#' obj = output_obj(M, 'result_1')
#'
#' # set a output as an object
#' output_obj(M, 'result_1') = entity(value = 15,type = 'numeric',name = 'result_1')
#' @export
setGeneric("output_obj",
    function(obj,name)standardGeneric("output_obj"))

#' @rdname output_obj
setGeneric("output_obj<-",
    function(obj,name,value)standardGeneric("output_obj<-"))

#' Verify output
#'
#' Verify that the name of a output is valid for an object
#' @param obj A model or iterator object derived from the *struct* class
#' @param name Name of output
#' @return TRUE if output name is valid, FALSE if not
#' @examples
#' M = example_model()
#' is_output(M,'result_1') # TRUE
#' is_output(M,'result_0')   # FALSE
#' @export
setGeneric("is_output",function(obj,name)standardGeneric("is_output"))

#' output name
#'
#' return a the name for a output, if available
#' @param obj A model or iterator object derived from the *struct* class
#' @param name Name of output
#' @return name of output
#'@examples
#' M = example_model()
#' output_name(M,'result_1')
#' @export
setGeneric("output_name",function(obj,name)standardGeneric("output_name"))

#' output list
#'
#' get/set a named list of outputs and their current value for an object
#' @param obj An object derived from struct_class
#' @param value A named list of outputs and corresponding values
#' @return A named list of outputs and corresponding values
#' @rdname output_list
#' @examples
#' M = example_model()
#' L = output_list(M)
#' @export
setGeneric("output_list",function(obj)standardGeneric("output_list"))

#' @rdname output_list
#' @return struct object
#' @examples
#' M = example_model()
#' output_list(M) = list('result_1' = DatasetExperiment(),'result_2' = DatasetExperiment())
#' @export
setGeneric("output_list<-",function(obj,value)standardGeneric("output_list<-"))

#####################################
###### chart class generics #####
#####################################

#' chart names
#'
#' Returns a list of valid charts for a struct object
#'
#' The chart_names method searches chart objects that specify the input object
#' type as an input.
#'
#' @param obj An object derived from the struct_class object
#' @param ret A string indicating whether a list of objects ('obj') or a list of chart 
#' names ('char') is returned. 'char' is default.
#' @return list of chart names, or a list of chart objects
#' @examples
#' M = example_model()
#' chart_names(M) # 'example_chart'
#' chart_names(M,'char') # as above
#' chart_names(M,'obj') # returns a list of chart objects
#' @export
setGeneric("chart_names",function(obj,ret='char')standardGeneric("chart_names"))

#' chart_plot
#'
#' Plots a chart object
#' 
#' The optional optional inputs depend on the input object/chart, but might 
#' include an additional dataset object or a second model object, for example.
#' @param obj A chart object
#' @param dobj An object derived from struct_class
#' @param ... optional inputs
#' @rdname chart_plot
#' @return a plot object
#' @examples
#' C = example_chart()
#' chart_plot(C,iris_DatasetExperiment())
#'
#' @export
setGeneric("chart_plot",function(obj, dobj, ...)standardGeneric("chart_plot"))


#################################
###### model class generics #####
#################################

#' Apply a model
#'
#' Applies a method to the input dataset
#' @param M a `method` object
#' @param D another object used by the first
#' @return Returns a modified method object
#' @rdname model_apply
#' @examples
#' M = example_model()
#' M = model_apply(M,iris_DatasetExperiment())
#' @export
setGeneric("model_apply",function(M,D)standardGeneric("model_apply"))


#' Train a model
#'
#' Trains a model using the input dataset
#' @param M a model object
#' @param D a dataset object
#' @return Returns a modified model object
#' @rdname train
#' @examples
#' M = example_model()
#' M = model_train(M,iris_DatasetExperiment())
#' @export
setGeneric("model_train",function(M,D)standardGeneric("model_train"))

#' Model prediction
#'
#' Apply a model using the input dataset_ Assumes the model is trained
#' first.
#' @param M a model object
#' @param D a dataset object
#' @return Returns a modified model object
#' @rdname predict
#' @examples
#' M = example_model()
#' M = model_predict(M,iris_DatasetExperiment())
#' @export
setGeneric("model_predict",function(M,D)standardGeneric("model_predict"))

#' Reverse preprocessing
#'
#' Reverse the effect of a preprocessing step on a dataset_
#' @param M a model object
#' @param D a dataset object
#' @return Returns a modified dataset object
#' @rdname model_reverse
#' @examples
#' M = example_model()
#' D = model_reverse(M,iris_DatasetExperiment())
#' @export
setGeneric("model_reverse",function(M,D)standardGeneric("model_reverse"))

#' Predicted output name
#'
#' get/set the prediction output for a model_ This determines which outputs from
#' this model are supplied as inputs to the next model when used in a model_seq
#' @param M a model object
#' @param value name of an output for this model
#' @return
#' \describe{
#' \item{\code{predicted_name}}{returns the name of the predicted output}
#' \item{\code{predicted_name<-}}{sets the name of the predicted output}
#' }
#' @rdname predicted_name
#' @examples
#' M = example_model()
#' predicted_name(M)
#' predicted_name(M) = 'result_2'
#' @export
setGeneric("predicted_name",function(M)standardGeneric("predicted_name"))

#' @rdname predicted_name
setGeneric("predicted_name<-",
    function(M,value)standardGeneric("predicted_name<-"))

#' Sequence input
#'
#' get/set the input parameter replaced by the output of the previous model in 
#' a model sequence. Default is "data" which passes the output as the data input
#' for methods such as \code{model_train} and \code{model_apply}.
#' @param M a model object
#' @param value name of an output for this model
#' @return
#' \describe{
#' \item{\code{seq_in}}{returns the name of the input parameter replaced 
#' when used in a model sequence}
#' \item{\code{seq_in<-}}{sets the name of the input parameter replaced 
#' when used in a model sequence}
#' }
#' @rdname seq_in
#' @examples
#' M = example_model()
#' seq_in(M)
#' seq_in(M) = 'value_1'
#' @export
setGeneric("seq_in",function(M)standardGeneric("seq_in"))

#' @rdname seq_in
setGeneric("seq_in<-",
    function(M,value)standardGeneric("seq_in<-"))

#' Prediction output
#'
#' returns the prediction output for a model_ This is supplied as input to the
#' next model when used in a model_seq
#' @param M a model object
#' @return The value returned varies depending on the output_
#' @rdname predicted
#' @examples
#' M = example_model()
#' M = model_train(M, iris_DatasetExperiment())
#' M = model_predict(M, iris_DatasetExperiment())
#' predicted(M)
#' @export
setGeneric("predicted",function(M)standardGeneric("predicted"))

######################################
###### model_seq class generics #####
######################################

#' Get/set models of a model_seq
#'
#' Returns the list of models in a model_seq object
#' @param ML a model_seq object
#' @param value a list containing only model objects
#' @return
#' \describe{
#' \item{\code{models(ML)}}{returns a list of models in the model sequence}
#' \item{\code{models(ML)<-}}{sets the list of models in the model sequence}
#' }
#' @rdname models
#' @examples
#' # Create a model sequence
#' ML = model_seq()
#' models(ML) = list(example_model(), example_model())
#' models(ML)
#' @export
setGeneric("models",function(ML)standardGeneric("models"))

#' @rdname models
setGeneric("models<-",function(ML,value)standardGeneric("models<-"))

#' write a dataset object to file
#'
#' @param object a dataset object
#' @param outfile the filename to write the data to
#' @param transpose TRUE or FALSE to transpose the output data
#' @rdname export_data
setGeneric("export_xlsx",function(object,outfile,transpose=TRUE)standardGeneric("export_xlsx"))

####################################
###### iterator class generics #####
####################################

#' Run iterator
#'
#' Runs an iterator, applying the chosen model multiple times.
#'
#' Running an iterator will apply the iterator a number of times to a dataset_
#' For example, in cross-validation the same model is applied multiple times to
#' the same data, splitting it into training and test sets. The input metric
#' object can be calculated and collected for each iteration as an output_
#' @param I an iterator object
#' @param D a dataset object
#' @param MET a metric object
#' @rdname iterator
#' @return Modified iterator object
#' @examples
#' D = iris_DatasetExperiment() # get some data
#' MET = metric()  # use a metric
#' I = example_iterator() # initialise iterator
#' models(I) = example_model() # set the model
#' I = run(I,D,MET) # run
#' @export
setGeneric("run",function(I,D,MET)standardGeneric("run"))

#' Evaluate an iterator
#'
#' Evaluates an iterator by e.g. averaging over all iterations. May be
#' deprecated in a future release as \code{evaluate} is applied by \code{run}
#' anyway.
#' @rdname iterator
#' @return Modified iterator object
#' @examples
#' D = iris_DatasetExperiment() # get some data
#' MET = metric()  # use a metric
#' I = example_iterator() # initialise iterator
#' models(I) = example_model() # set the model
#' I = run(I,D,MET) # run
#' I = evaluate(I,MET) # evaluate
setGeneric("evaluate",function(I,MET)standardGeneric("evaluate"))

#' get/set output name as prediction output for a model
#'
#' get/set the prediction output for a model_ This determines which outputs
#' from this model are supplied as inputs to the next model when used in
#' a model_seq
#' @param M an iterator object
#' @param I an iterator object
#' @param value name of an output for iterator M
#' @rdname result_name
#' @return
#' \describe{
#' \item{\code{result_name(M)}}{returns the name of the output for this iterator
#' (equivalent to \code{predicted} for model objects)}
#' \item{\code{result_name(I)<-}}{sets the default output for an iterator}
#' }
#' @examples
#' I = example_iterator() # initialise iterator
#' result_name(I)
#' result_name(I) = 'result_1'
#' @export
setGeneric("result_name",function(M)standardGeneric("result_name"))

#' @rdname result_name
setGeneric("result_name<-",function(I,value)standardGeneric("result_name<-"))

#' Iterator result
#'
#' Returns the results of an iterator. This is used to control model flow in a
#' similar way to \code{predict} for model and model_seq objects.
#' @param M an iterator object
#' @rdname result
#' @return the returned output varies with the algorithm implemented
#' @examples
#' D = iris_DatasetExperiment() # get some data
#' MET = metric()  # use a metric
#' I = example_iterator() # initialise iterator
#' models(I) = example_model() # set the model
#' I = run(I,D,MET) # run
#' result(I)
#' @export
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
#' @examples
#' MET = metric()
#' calculate(MET)
#' @export
setGeneric("calculate",function(obj, ...)standardGeneric("calculate"))

#' get the value for a metric
#'
#' @return value the calculated value of a metric
#' @rdname metric
#' @export
setGeneric("value",function(obj)standardGeneric("value"))

#' set the value for a metric
#'
#' @rdname metric
setGeneric("value<-",function(obj,value)standardGeneric("value<-"))

#' get the max value vector length for an entity
#'
#' @param obj an entity object
#' @return max value vector length for an entity
#' @rdname entity
#' @export
setGeneric("max_length",function(obj)standardGeneric("max_length"))

#' set the value for a metric
#'
#' @rdname metric
setGeneric("max_length<-",function(obj,value)standardGeneric("max_length<-"))

####################################
###### stato class generics #####
####################################

#' get the stato_id for an object
#'
#' @return id the stato id
#' @param obj stato_class object
#' @rdname stato
#' @examples
#' M = example_model()
#' stato_id(M)
#' stato_name(M)
#' stato_definition(M)
#' stato_summary(M)
#' @export
setGeneric("stato_id",function(obj)standardGeneric("stato_id"))

#' get the stato name for an object
#'
#' @return name the stato name
#' @rdname stato
#' @export
setGeneric("stato_name",function(obj)standardGeneric("stato_name"))

#' get the stato name for an object
#'
#' @return def the stato description
#' @rdname stato
#' @export
setGeneric("stato_definition",function(obj)standardGeneric("stato_definition"))

#' display a stato summary of the object
#'
#' @rdname stato
#' @export
setGeneric("stato_summary",function(obj)standardGeneric("stato_summary"))

#' Convert a DatasetExperiment to a SummarizedExperiment
#' 
#' Converts a DatasetExperiment to SummarizedExperiment. The assay data is 
#' transposed, and colData and rowData switched to match. struct specific
#' slots such as "name" and "description" are stored in the metaData.
#' @param obj a DatasetExperiment object
#' @return a SummarizedExperiment object
#' @export
setGeneric("as.SummarizedExperiment",function(obj)standardGeneric("as.SummarizedExperiment"))

#' Convert a SummarizedExperiment to DatasetExperiment
#' 
#' Converts a SummarizedExperiment to DatasetExperiment. The assay data is 
#' transposed, and colData and rowData switched to match. struct specific
#' slots such as "name" and "description" are extracted from the metaData.
#' @param obj a SummarizedExperiment object
#' @return a DatasetExperiment object
#' @export
setGeneric("as.DatasetExperiment",function(obj)standardGeneric("as.DatasetExperiment"))

#' Convert to code
#' 
#' Prints a block of code that can be used to replicate the input object.
#' 
#' @param M a struct model, model_seq or iterator object
#' @param start text prepended to the code. Default is "M = "
#' @param mode "compact" will use the least amount of lines, "expanded" will
#' put each object and input on a new line. "neat" will produce an output
#' somewhere between "compact" and "extended".
#' @return A string of code to reproduce the input object.
#' @export
#' @rdname as.code
#' @examples 
#' M = example_model(value_1 = 10)
#' as.code(M)
setGeneric('as.code',function(M,start='M = ',mode='compact')standrdGeneric("as.code"))

#' convert to data.frame
#'
#' Most often used with univariate statistics to gather all the different outputs in a consistent format.
#' 
#' @param M a struct object
#' @param ... other inputs passed through this function
#' @return a data.frame containing outputs from an object
#' @export
setGeneric("as_data_frame",function(M,...)standardGeneric("as_data_frame"))


#' Citations for an object
#' 
#' All \code{struct} objects have a "citations" slot, which is a list of
#' references in bibtex format. The \code{citations} method gathers
#' citations from an object and all \code{struct} objects that it inherits to generate
#' a complete list.
#' @param obj a struct object
#' @return a character array of citations
#' @examples 
#' D = iris_DatasetExperiment()
#' D$citations # the list specifically defined for this object
#' citations(D) # the list for this object and all inherited ones
#' @rdname citations
#' @export
setGeneric("citations",function(obj)standardGeneric("citations"))

#' Libraries for an object
#' 
#' All \code{struct} objects have a "libraries" slot, which is a character array of
#' libraries required to use the object. The \code{libraries} method gathers
#' libraries from an object and all \code{struct} objects that it inherits to generate
#' a complete list.
#' @param obj a struct object
#' @return a character array of R packages needed by the object
#' @examples 
#' M = example_model()
#' libraries(M)
#' @rdname libraries
#' @export
setGeneric("libraries",function(obj)standardGeneric("libraries"))

#' Ontology for an object
#' 
#' All \code{struct} objects have an "ontology" slot, which is a list of
#' ontology items for the object. The \code{ontology} method gathers
#' ontology items from an object and all \code{struct} objects that it inherits to generate
#' a complete list.
#' @param obj a struct object
#' @param cache a named list of ontology_terms for offline use. Terms from the cache are search 
#' based on the name of the list items matching the ontology id. If cache=NULL then the OLS API is used to lookup terms.
#' @examples 
#' M = example_model()
#' ontology(M,cache=NULL)
#' @rdname ontology
#' @export
setGeneric("ontology",function(obj,cache=NULL)standardGeneric("ontology"))
