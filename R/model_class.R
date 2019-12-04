#' model class
#'
#' A class for models that can be trained/applied to datasets e.g. PCA, PLS etc.
#' Also used for preprocessing steps that require application to test sets.
#' not intended to be called directly, this class should be inherited to provide
#' functionality for method-specific classes.
#' @export model
#' @param M a model object
#' @param D a dataset object
#' @param value value
#' @include generics.R    parameter_class.R output_class.R dataset_class.R
#' @examples
#' M = model()

model<-setClass(
    "model",
    contains = c('struct_class','parameter_class','outputs_class'),
    slots = c(type = 'character',
        predicted = 'character',
        seq_in = 'character'
    ),
    prototype = list(seq_in = 'data')
)

#' @describeIn model train the model using input data
#' @export
#' @examples
#' D = dataset()
#' M = model()
#' M = model.train(M,D)
#' @return trained model object
setMethod(f = "model.train",
    signature = c("model","dataset"),
    definition = function(M,D)
    {
        warning('no training implemented for this model')
        return(M)
    }
)

#' @describeIn model apply the model to input data
#' @export
#' @examples
#' D = dataset()
#' M = model()
#' M = model.train(M,D)
#' M = model.predict(M,D)
#' @return model object with test set results
setMethod(f = "model.predict",
    signature = c("model","dataset"),
    definition = function(M,D)
    {
        return(M)
    }
)

#' @describeIn model trains and tests the data using the input model
#' @export
#' @examples
#' D = dataset()
#' M = model()
#' M = model.apply(M,D)
#' @return trained model object
setMethod(f = "model.apply",
    signature = c("model","dataset"),
    definition = function(M,D)
    {
        M = model.train(M,D)
        M = model.predict(M,D)
        return(M)
    }
)

#' @describeIn model reverse the model for preprocessing steps
#' @export
#' @examples
#' D = dataset()
#' M = model()
#' M = model.train(M,D)
#' M = model.predict(M,D)
#' M = model.reverse(M,D)
#' @return dataset dataset object with the reverse model applied
setMethod(f = "model.reverse",
    signature = c("model","dataset"),
    definition = function(M,D)
    {
        return(D)
    }
)

#' @describeIn model get prediction output from model
#' @export
#' @examples
#' D = dataset()
#' M = example_model()
#' M = model.train(M,D)
#' M = model.predict(M,D)
#' p = predicted(M)
#' @return the predicted output, as specified by predicted.name
setMethod(f = 'predicted',
    signature = c('model'),
    definition = function(M) {
        return(output.value(M,predicted.name(M)))
    }
)

#' @describeIn model get prediction output name for model
#' @export
#' @examples
#' M = example_model()
#' predicted.name(M)
#' @return the id of the output returned by predicted()
setMethod(f = 'predicted.name',
    signature = c('model'),
    definition = function(M) {
        return(M@predicted)
    }
)

#' @describeIn model set prediction output from model
#' @export
#' @examples
#' M = example_model()
#' predicted.name(M) = 'result_2'
#' @return the modified model object
setMethod(f = 'predicted.name<-',
    signature = c('model','character'),
    definition = function(M,value) {
        M@predicted = value
        return(M)
    }
)

#' @export
setMethod(f = "show",
    signature = c("model"),
    definition = function(object) {
        callNextMethod()
        cat('predicted:     ',predicted.name(object),'    (', class(predicted(object)),')\n',sep='')
        cat('seq_in:        ',object@seq_in,'\n',sep = '')
        cat('\n')
    }
)
