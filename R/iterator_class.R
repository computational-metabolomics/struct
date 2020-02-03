#' iterator class
#'
#' A class for iterative approaches that involve the training/prediction of a
#' model multiple times. Not intended to be called directly, this class should
#' be inherited to provide functionality for method-specific classes.
#' @export iterator
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
#' @param ... named slots and their values.
#' @rdname iterator
iterator = function(...) {
    # new object
    out = .iterator(...)
    return(out)
}




.iterator<-setClass(
    "iterator",
    contains = c('struct_class'),
    slots = c(
        type = 'character',
        models = 'ANY', # any here, but types enforced by e.g. resampler
        result = 'character')
)


#' @rdname iterator
#' @export
#' @examples
#' D = DatasetExperiment()
#' MET = metric()
#' I = iterator() * model()
#' I = run(I,D,MET)
#'
setMethod(f = "run",
    signature = c("iterator","DatasetExperiment",'metric'),
    definition = function(I,D,MET = NULL) {
        warning('the base iterator function was called, not the one defined for
            your specific iterator')
        return(I)
    }
)


#' @rdname iterator
#' @export
setMethod(f = "evaluate",
    signature = c("iterator","metric"),
    definition = function(I,MET) {
        warning('no evaluate fcn for this object. Evaluate is depricated and may
            be removed in a future release')
        return(I)
    }
)

#' @rdname iterator
#' @export
setMethod(f = "models",
    signature = c("iterator"),
    definition = function(ML) {
        return(ML@models)
    }
)

setClassUnion("model_OR_iterator", c("model", "iterator","model_seq"))

#' @rdname iterator
#' @export
setMethod(f = "models<-",
    signature = c("iterator",'model_OR_iterator'),
    definition = function(ML,value) {
        ML@models = value
        return(ML)
    }
)

#' @rdname iterator
#' @export
#' @examples
#' I = iterator()
#' result_name(I) = 'example'
#' @return the modified model object
setMethod(f = 'result_name<-',
    signature = c('iterator','character'),
    definition = function(I,value) {
        I@result = value
        return(I)
    }
)

#' @rdname iterator
#' @export
setMethod(f = 'result',
    signature = c('iterator'),
    definition = function(M) {
        return(output_value(M,result_name(M)))
    }
)

#' @rdname iterator
#' @export
setMethod(f = 'result_name',
    signature = c('iterator'),
    definition = function(M) {
        return(M@result)
    }
)

#' @rdname iterator
#' @export
setMethod(f = '*',
    signature = c(e1 = 'iterator',e2 = 'model_OR_iterator'),
    definition = function(e1,e2) {
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

#' @rdname iterator
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
    definition = function(x,i) {
        return(models(x)[i])
    }
)

#' @rdname iterator
#' @export
#' @examples
#' MS = model() + model()
#' I = iterator() * MS
#' I[2] = model() # sets the second model to model()
#'
#' @return model sequence with the model at index i replaced
setMethod(f = "[<-",
    signature = "iterator",
    definition = function(x,i,value) {
        if (!is(value,'model')) {
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
        value_1 = 'numeric',
        value_2 = 'numeric',
        result_1 = 'numeric',
        result_2 = 'numeric'
    ),
    prototype = list(result = 'result_1',
        .params=c('value_1','value_2'),
        .outputs=c('result_1','result_2')
    )
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
#' D = iris_DatasetExperiment()
#' MET = metric()
#' I = run(I,D,MET)
#'
setMethod(f = 'run',
    signature = c('example_iterator','DatasetExperiment','metric'),
    definition = function(I,D,MET) {
        I$result_1 = 3.142
        calculate(MET)
        return(I)
    })


setMethod(f = 'show',
    signature = c('iterator'),
    definition = function(object) {
        callNextMethod()
        
        if (is(models(object),'model_seq')) {
            cat('models:        ','a model_seq with ', length(models(object)),' steps\n',sep='')
        } else {
            cat('models:        ','a ',class(models(object)), 'object\n',sep='')
        }
        cat('result:        ',result_name(object),'    (', class(result(object)),')\n',sep='')
        cat('\n')
    }
)
