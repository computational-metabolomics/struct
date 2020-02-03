#' model class
#'
#' A class for models that can be trained/applied to datasets e.g. PCA, PLS etc.
#' Also used for preprocessing steps that require application to test sets.
#' not intended to be called directly, this class should be inherited to provide
#' functionality for method-specific classes.
#' 
#' @section \code{predicted} slot:
#' The "predicted" slot is a slots for use by users to control the flow of model
#' sequences. The \code{predicted()} function is used to return a default output and
#' from a model. Typically it is a DatasetExperiment object that is passed directly
#' into the next model in a sequence as the data for that model.
#' 
#' @section \code{seq_in} slot:
#' In a sequence of models (see model_seq) the "predicted" slot is connected to the 
#' DatasetExperiment input of the next model. \code{seq_in} can be used to control
#' flow and connect the "predicted" output to the input parameter of the next model.
#' Default is the keyword 'data', and can otherwise be replaced by any input slot 
#' from the model.
#' 
#' @export model
#' @param M A struct model object
#' @param D A DatasetExperiment object
#' @param value The value to assign 
#' @param predicted The name of an output slot to return when using \code{predicted()} (see details)
#' @param seq_in the name of an output slot to connect with the "predicted" output 
#' of another model (see details)
#' @include generics.R parameter_class.R output_class.R
#' @examples
#' M = model()
#' @param ... named slots and their values.
#' @rdname model
model = function(predicted=character(0),seq_in='data',...) {
    # new object
    out = .model(predicted = predicted,
        seq_in = seq_in,
        ...)
    return(out)
}

.model<-setClass(
    "model",
    contains = c('struct_class'),
    slots = c(
        predicted = 'character',
        seq_in = 'character'
    ),
    prototype = list(seq_in = 'data')
)

#' @rdname model
#' @export
#' @examples
#' D = DatasetExperiment()
#' M = model()
#' M = model_train(M,D)
#' @return trained model object
setMethod(f = "model_train",
    signature = c("model","DatasetExperiment"),
    definition = function(M,D) {
        warning('no training implemented for this model')
        return(M)
    }
)

#' @rdname model
#' @export
#' @examples
#' D = DatasetExperiment()
#' M = model()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' @return model object with test set results
setMethod(f = "model_predict",
    signature = c("model","DatasetExperiment"),
    definition = function(M,D) {
        return(M)
    }
)

#' @rdname model
#' @export
#' @examples
#' D = DatasetExperiment()
#' M = model()
#' M = model_apply(M,D)
#' @return trained model object
setMethod(f = "model_apply",
    signature = c("model","DatasetExperiment"),
    definition = function(M,D) {
        M = model_train(M,D)
        M = model_predict(M,D)
        return(M)
    }
)

#' @rdname model
#' @export
#' @examples
#' D = DatasetExperiment()
#' M = model()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' M = model_reverse(M,D)
#' @return dataset dataset object with the reverse model applied
setMethod(f = "model_reverse",
    signature = c("model","DatasetExperiment"),
    definition = function(M,D) {
        return(D)
    }
)

#' @rdname model
#' @export
#' @examples
#' D = DatasetExperiment()
#' M = example_model()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' p = predicted(M)
#' @return the predicted output, as specified by predicted_name
setMethod(f = 'predicted',
    signature = c('model'),
    definition = function(M) {
        if (length(predicted_name(M))==0) {
            warning('"predicted" has not been set')
            return(NA)
        }
        if (is.na(predicted_name(M))) {
            warning('"predicted" is set to NA')
            return(NA)
        }
        if (is.null(predicted_name(M))) {
            warning('"predicted" is set to NULL')
            return(NA)
            
        }
        # we can try to return the slot
        return(output_value(M,predicted_name(M)))
    }
    
)

#' @rdname model
#' @export
#' @examples
#' M = example_model()
#' predicted_name(M)
#' @return the id of the output returned by predicted()
setMethod(f = 'predicted_name',
    signature = c('model'),
    definition = function(M) {
        return(M@predicted)
    }
)

#' @rdname model
#' @export
#' @examples
#' M = example_model()
#' predicted_name(M) = 'result_2'
#' @return the modified model object
setMethod(f = 'predicted_name<-',
    signature = c('model','character'),
    definition = function(M,value) {
        M@predicted = value
        return(M)
    }
)

setMethod(f = "show",
    signature = c("model"),
    definition = function(object) {
        callNextMethod()
        cat('predicted:     ',predicted_name(object),'\n',sep = '')
        cat('seq_in:        ',object@seq_in,         '\n',sep = '')
        cat('\n')
    }
)


