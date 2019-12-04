#' iterator class
#'
#' A class for iterative approaches that involve the training/prediction of a
#' model multiple times. Not intended to be called directly, this class should
#' be inherited to provide functionality for method-specific classes.
#' @export iterator
#' @inheritParams run
#' @param ML a model sequence object
#' @param M a model object
#' @param value value
#' @param e1 an iterator object
#' @param e2 an iterator or a model object
#' @include generics.R parameter_class.R output_class.R model_class.R
#' @include metric_class.R model_list_class.R
#' @examples
#' I = iterator()
#' I = iterator() * model()

iterator<-setClass(
    "iterator",
    contains = c('struct_class','parameter_class','outputs_class'),
    slots = c(type = 'character',
        models = 'ANY', # any here, but types enforced by e.g. resampler
        result = 'character')
)


#' @describeIn iterator run a model/model.seq mutliple times for the input data
#' @export
#' @examples
#' D = dataset()
#' MET = metric()
#' I = iterator() * model()
#' I = run(I,D,MET)
#'
setMethod(f = "run",
    signature = c("iterator","dataset",'metric'),
    definition = function(I,D,MET = NULL)
    {
        warning('the base iterator function was called, not the one defined for
            your specific iterator')
        return(I)
    }
)


#' @describeIn iterator evaluate the performance of a model/model.seq using
#' the input metric
#' @export
setMethod(f = "evaluate",
    signature = c("iterator","metric"),
    definition = function(I,MET)
    {
        warning('no evaluate fcn for this object. Evaluate is depricated and may
            be removed in a future release')
        return(I)
    }
)

#' @describeIn iterator get the model/model.seq for an iterator object
#' @export
setMethod(f = "models",
    signature = c("iterator"),
    definition = function(ML)
    {
        return(ML@models)
    }
)

setClassUnion("model_OR_iterator", c("model", "iterator","model.seq"))

#' @describeIn iterator set the model/model.seq to be run for multiple
#' iterations
#' @export
setMethod(f = "models<-",
    signature = c("iterator",'model_OR_iterator'),
    definition = function(ML,value)
    {
        ML@models = value
        return(ML)
    }
)

#' @describeIn iterator set result output from iterator
#' @export
#' @examples
#' I = iterator()
#' result.name(I) = 'example'
#' @return the modified model object
setMethod(f = 'result.name<-',
    signature = c('iterator','character'),
    definition = function(I,value)
    {
        I@result = value
        return(I)
    }
)

#' @describeIn iterator get prediction output from iterator
#' @export
setMethod(f = 'result',
    signature = c('iterator'),
    definition = function(M)
    {
        return(output.value(M,result.name(M)))
    }
)

#' @describeIn iterator get prediction output name for iterator
#' @export
setMethod(f = 'result.name',
    signature = c('iterator'),
    definition = function(M)
    {
        return(M@result)
    }
)

#' @describeIn iterator combine an interator with other model or interator
#' objects
#' @export
setMethod(f = '*',
    signature = c(e1 = 'iterator',e2 = 'model_OR_iterator'),
    definition = function(e1,e2)
    {
        m = models(e1)
        if (is(m,'iterator')) {
            models(e1) = m*e2
        } else {
            #print(paste('putting',class(e2),'into',class(e1),sep = ' '))
            models(e1) = e2
        }
        return(e1)
    }
)

#' @describeIn iterator get model by index
#' @param x a sequence object
#' @param i index into sequence
#' @export
#' @examples
#' MS = model() + model()
#' I = iterator() * MS
#' I[2] # returns the second model() object
#'
#' @return model at the given index in the sequence
setMethod(f = "[",
    signature = "iterator",
    definition = function(x,i){
            return(models(x)[i])
    }
)

#' @describeIn iterator set model by index
#' @export
#' @examples
#' MS = model() + model()
#' I = iterator() * MS
#' I[2] = model() # sets the second model to model()
#'
#' @return model sequence with the model at index i replaced
setMethod(f = "[<-",
    signature = "iterator",
    definition = function(x,i,value){
        if (!is(value,'model'))
        {
            stop('value must be a model')
        }
        models(x)[i] = value
        return(x)
    }
)


#' Example iterator
#'
#' An example iterator for testing
#' @export example_iterator
#' @return test iterator object
#' @param I example_iterator object
#' @param D dataset object
#' @param MET metric object
#' @rdname example_iterator
#' @include generics.R parameter_class.R output_class.R model_class.R
#' @include metric_class.R model_list_class.R
#' @examples
#' I = example_iterator()
#'
example_iterator = setClass('example_iterator',
    contains = 'iterator',
    slots = c(
        params.value_1 = 'numeric',
        params.value_2 = 'numeric',
        outputs.result_1 = 'numeric',
        outputs.result_2 = 'numeric'
    ),
    prototype = list(result = 'result_1')
)

#' run example
#'
#' runs the example iterator, which just returns a value of 3.142
#' @export
#' @return dataset object
#' @rdname example_iterator
#' @include generics.R parameter_class.R output_class.R model_class.R
#' @include metric_class.R model_list_class.R
#' @examples
#' I = example_iterator()
#' D = iris_dataset()
#' MET = metric()
#' I = run(I,D,MET)
#'
setMethod(f = 'run',
    signature = c('example_iterator','dataset','metric'),
    definition = function(I,D,MET){
        I$result_1 = 3.142
        calculate(MET)
        return(I)
    })
