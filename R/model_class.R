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
#' from the model. The slot \code{seq_fcn} can be used to apply a transformation to 
#' the output before it is used as an input. This allows you to e.g. convert between types,
#' extract a single column from a data.frame etc.
#' 
#' 
#' @export model
#' @param M A struct model object
#' @param D A DatasetExperiment object
#' @param value The value to assign 
#' @param predicted The name of an output slot to return when using \code{predicted()} (see details)
#' @param seq_in the name of an output slot to connect with the "predicted" output 
#' of another model (see details)
#' @param seq_fcn a function to apply to seq_in before inputting into the next model. 
#' Typically used to extract a single column, or convert from factor to char etc.
#' @include generics.R parameter_class.R output_class.R
#' @examples
#' M = model()
#' @param ... named slots and their values.
#' @rdname model
model = function(predicted=character(0),seq_in='data',seq_fcn=function(x){return(x)},...) {
    # new object
    out = .model(predicted = predicted,
        seq_in = seq_in,
        seq_fcn = seq_fcn,
        ...)
    return(out)
}

.model<-setClass(
    "model",
    contains = c('struct_class'),
    slots = c(
        predicted = 'character',
        seq_in = 'character',
        seq_fcn = 'function'
    ),
    prototype = list(
        seq_in = 'data',
        seq_fcn=function(x){return(x)}
    )
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
#' D = DatasetExperiment()
#' M = example_model()
#' seq_in(M) = 'data'
#' @return the id of the input parameter to be replaced by the \code{predicted} 
#' output of the previous model in a model sequence. Reserved keyword 'data' 
#' means that the input data used by \code{model_train}, \code{model_apply} etc is used. 
#' \code{seq_in = 'data'} is the default setting. 
setMethod(f = 'seq_in',
    signature = c('model'),
    definition = function(M) {
        return(M@seq_in)
    }
    
)

#' @rdname model
#' @export
#' @examples
#' M = example_model()
#' seq_in(M) = 'value_1'
#' @return the modified model object
setMethod(f = 'seq_in<-',
    signature = c('model','character'),
    definition = function(M,value) {
        if (value %in% param_ids(M) | value=='data') {
            M@seq_in = value
        } else {
            stop('"', value, '" is not a valid input parameter id for',
                ' a ', class(M)[1], ' object.')
        }
        return(M)
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
        if (value %in% output_ids(M)) {
            M@predicted = value
        } else {
            stop('"', value, '" is not a valid output id for',
                ' a ', class(M), ' object.')
        }
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


#' @rdname as.code
#' @export
#' @examples
#' M = example_model()
#' as.code(M)
#' @return a string of code to reproduce the model
setMethod(f = 'as.code',
    signature = c('model'),
    definition = function(M,start = 'M = ',mode = 'compact') {
        .as_code(M,start,mode)
    }
)



.as_code = function(M,start='M = ',mode = 'compact') {
    
    if (!(mode %in% c('compact','neat','expanded','full'))) {
        stop(paste0('unknown option "', mode , '" for as.code()'))
    }
    str=start
    # model object name
    str=paste0(str,class(M)[1],'(')
    
    # parameters
    P = param_ids(M)
    
    # add seq_in if not equal to data
    if (is(M,'model')) {
        if (M@seq_in != 'data' | mode=='full') {
        P=c(P,'seq_in')
        }
    }
    # add predicted if its not the default
    if (is(M,'model')) {
        N=new_struct(class(M)[1])
        if  (predicted_name(N) != predicted_name(M) | mode=='full') {
            P=c(P,'predicted')
        }
    }
    
    if (mode != "compact") {
        str=paste0(str,'\n')
        indent=nchar(start)+2
    } else {
        indent=(nchar(start)+1)+nchar(class(M)[1])
    }
    
    for (p in seq_len(length(P))) {
        if (p>1 | mode!="compact") {
            str=paste0(str,paste0(rep(' ',indent),collapse=''))
        } 
        
        if (P[p]=='seq_in') {
            str=paste0(str,P[p], ' = "', seq_in(M), '"')
        } else if (P[p]=='predicted') {
            str=paste0(str,P[p], ' = "', predicted_name(M), '"')
        } else if (is(param_value(M,P[p]),'character')) {
            str=paste0(str,P[p], ' = "', as.character(param_value(M,P[p])), '"')
        } else {
            str=paste0(str,P[p], ' = ', as.character(param_value(M,P[p])))
        }
        
        
        if (p==length(P)) {
            if (mode=='expanded') {
                str=paste0(str,'\n',paste0(rep(' ',indent-2),collapse=''))
            }
            
            
            str=paste0(str,')')
            
            
        } else {
            str=paste0(str,',\n')
        }
    }
    
    return(str)
}

# autocompletion, return sample_meta column names
#' @export
#' @rdname autocompletion
#' @method .DollarNames model
.DollarNames.model <- function(x, pattern = "") {
    .DollarNames.struct_class(x,pattern)
}

#' @export 
#' @rdname autocompletion
setMethod('.DollarNames','model',.DollarNames.model)

