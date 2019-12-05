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
#' @param ... named slots and their values.
#' @rdname model
model = function(...) {
    # new object
    out = .model()
    # initialise
    out = .initialize_struct_class(out,...)
    return(out)
}

.model<-setClass(
    "model",
    contains = c('struct_class','parameter_class','outputs_class'),
    slots = c(type = 'character',
        predicted = 'character',
        seq_in = 'character'
    ),
    prototype = list(seq_in = 'data')
)

#' @rdname model
#' @export
#' @examples
#' D = dataset()
#' M = model()
#' M = model_train(M,D)
#' @return trained model object
setMethod(f = "model_train",
    signature = c("model","dataset"),
    definition = function(M,D) {
        warning('no training implemented for this model')
        return(M)
    }
)

#' @rdname model
#' @export
#' @examples
#' D = dataset()
#' M = model()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' @return model object with test set results
setMethod(f = "model_predict",
    signature = c("model","dataset"),
    definition = function(M,D) {
        return(M)
    }
)

#' @rdname model
#' @export
#' @examples
#' D = dataset()
#' M = model()
#' M = model_apply(M,D)
#' @return trained model object
setMethod(f = "model_apply",
    signature = c("model","dataset"),
    definition = function(M,D) {
        M = model_train(M,D)
        M = model_predict(M,D)
        return(M)
    }
)

#' @rdname model
#' @export
#' @examples
#' D = dataset()
#' M = model()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' M = model_reverse(M,D)
#' @return dataset dataset object with the reverse model applied
setMethod(f = "model_reverse",
    signature = c("model","dataset"),
    definition = function(M,D) {
        return(D)
    }
)

#' @rdname model
#' @export
#' @examples
#' D = dataset()
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
        cat('predicted:     ',predicted_name(object),'    (', class(predicted(object)),')\n',sep='')
        cat('seq_in:        ',object@seq_in,'\n',sep = '')
        cat('\n')
    }
)
