#' model_seq class
#'
#' A class for (ordered) lists of models
#'
#' @export model_seq
#' @param M a model object
#' @param D a dataset object
#' @param x a model_seq object
#' @param i index
#' @param ML a model_seq object
#' @param e1 a model or model_seq object
#' @param e2 a model or model_seq object
#' @param value value
#' @include generics.R parameter_class.R output_class.R struct_class.R
#' @include model_class.R
#' @examples
#' MS = model_seq()
#' MS = model() + model()
#' @param ... named slots and their values.
#' @rdname model_seq
model_seq = function(...) {
    # new object
    out = .model_seq(...)
    return(out)
}


.model_seq<-setClass(
    "model_seq",
    contains = c('struct_class'),
    slots = c(models = 'list')
)


#' @rdname model_seq
#' @export
#' @examples
#' MS = example_model() + example_model()
#' MS = model_train(MS,DatasetExperiment())
#' @return model sequence
setMethod(f = "model_train",
    signature = c("model_seq","DatasetExperiment"),
    definition = function(M,D) {
        # for each model in the list
        # for first in list the input D is the data object
        for (i in seq_len(length(M))) {
            if (M[i]@seq_in != 'data') {
                # apply transformation
                S=M[i]@seq_fcn(S)
                # set
                param_value(M[i],M[i]@seq_in)=S
            }
            
            # train the model on the output of the previous model
            M[i] = model_train(M[i],D)
            # apply the model to the output of the previous model
            M[i] = model_predict(M[i],D)
            # set the output of this model as the input for the next model
            S = predicted(M[i])
            
            if (is(S,'DatasetExperiment')) {
                # if its a dataset then update current D
                D = predicted(M[i])
            }
        }
        return(M)
    }
)

#' @rdname model_seq
#' @export
#' @examples
#' D = DatasetExperiment()
#' MS = example_model() + example_model()
#' MS = model_train(MS,D)
#' MS = model_predict(MS,D)
#' @return model sequence
setMethod(f = "model_predict",
    signature = c("model_seq",'DatasetExperiment'),
    definition = function(M,D) {
        # for the first model the input use the input data
        S=D
        L = length(M) # number of models
        for (i in seq_len(L)) {
            # apply the model the output of the previous model
            M[i] = model_predict(M[i],D)

            # get the output of this model
            S = predicted(M[i])
            if (is(S,'DatasetExperiment')) {
                # keep the previous output
                penultimate = D                
                # update data for the next model
                D = S
                # otherwise the previous data output is used
            }
        }
        
        # if regression, reverse the processing to get predictions
        # on the same scale as the input data
        if (M[L]$type == 'regression') {
            # put the predictions into the penultimate dataset object
            penultimate$sample_meta[,M[L]$factor_name] = S
            # apply the reverse models (only works if all are preprocess models)
            for (k in seq(L-1,1,-1)) {
                penultimate = model_reverse(M[k],penultimate)
            }
            # put the update predictions into the last model
            pred = predicted_name(M[L]) # name of predicted output
            output_value(M[L],pred) = as.data.frame(penultimate$sample_meta[,M[L]$factor_name])
        }
        return(M)
    }
)

#' @rdname model_seq
#' @export
#' @examples
#' MS = model() + model()
#' MS[2]
#'
#' @return model at the given index in the sequence
setMethod(f = "[",
    signature = "model_seq",
    definition = function(x,i) {
        return(x@models[[i]])
    }
)

#' @rdname model_seq
#' @export
#' @examples
#' MS = model() + model()
#' MS[3] = model()
#'
#' @return model sequence with the model at index i replaced
setMethod(f = "[<-",
    signature = "model_seq",
    definition = function(x,i,value) {
        if (!is(value,'model')) {
            stop('value must be a model')
        }
        x@models[[i]] = value
        return(x)
    }
)

#' @rdname model_seq
#' @export
#' @examples
#' MS = model() + model()
#' L = models(MS)
#'
#' @return a list of models in the sequence
setMethod(f = 'models',
    signature = 'model_seq',
    definition = function(ML) {
        return(ML@models)
    }
)

#' @rdname model_seq
#' @export
#' @examples
#' MS = model_seq()
#' L = list(model(),model())
#' models(MS) = L
#'
#' @return a model sequence containing the input models
setMethod(f = 'models<-',
    signature = c('model_seq','list'),
    definition = function(ML,value) {
        # check that all items in list are models
        ism = lapply(X = value,FUN = is,class2 = 'model')
        if (!all(unlist(ism))) {
            stop('all items in list must be a model')
        }
        # if they are all models then add them to the object
        ML@models = value
        return(ML)
    }
)

#' @rdname model_seq
#' @export
#' @examples
#' MS = model() + model()
#' length(MS) # 2
#'
#' @return the number of models in the sequence
setMethod(f = 'length',
    signature = 'model_seq',
    definition = function(x) {
        return(length(x@models))
    }
)

#' 

setMethod(f = 'show',
    signature = 'model_seq',
    definition = function(object) {
        cat('A model_seq object containing:\n\n')
        if (length(object) == 0) {
            cat('no models')
            return()
        }
        for (i in seq_len(length(object))) {
            cat('[',i,']\n',sep='')
            show(object[i])
        }
    }
)



setClassUnion("model_OR_model_seq", c("model", "model_seq"))

#' @rdname model_seq
#' @export
#' @examples
#' MS = model() + model()
#' M = model()
#' MS = M + MS
#'
#' @return a model sequence with the additional model appended to the front of
#' the sequence
setMethod("+",
    signature(e1 = 'model',e2 = 'model_seq'),
    definition = function(e1,e2) {
        m = models(e2)
        m = c(e1,m)
        models(e2) = m
        return(e2)
    }
)

#' @rdname model_seq
#' @export
#' @examples
#' MS = model() + model()
#' M = model()
#' MS = MS + M
#'
#' @return a model sequence with the additional model appended to the end of the
#' sequence
setMethod("+",
    signature(e1 = 'model_seq',e2 = 'model'),
    definition = function(e1,e2) {
        m = models(e1)
        m = c(m,e2)
        models(e1) = m
        return(e1)
    }
)

#' @rdname model_seq
#' @export
#' @examples
#' MS = model() + model()
#'
#' @return a model sequence
setMethod("+",
    signature(e1 = 'model',e2 = 'model'),
    definition = function(e1,e2) {
        ML = model_seq(models = c(e1,e2))
        return(ML)
    }
)

#' @rdname model_seq
#' @export
#' @examples
#' D = DatasetExperiment()
#' M = example_model()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' p = predicted(M)
#' @return the predicted output of the last model in the sequence
setMethod(f = 'predicted',
    signature = c('model_seq'),
    definition = function(M) {
        # return the predicted ooutput from the last model
        L = length(M)
        return(output_value(M[L],predicted_name(M[L])))
    }
)


#' @rdname model_seq
#' @export
#' @examples
#' D = DatasetExperiment()
#' MS = example_model() + example_model()
#' MS = model_apply(MS,D)
#'
setMethod(f = "model_apply",
    signature = c("model_seq","DatasetExperiment"),
    definition = function(M,D) {
        # for each method in the list
        S = D # for first in list the input D is the data object
        
        for (i in seq_len(length(M))) {
            if (M[i]@seq_in != 'data') {
                # apply transformation
                S=M[i]@seq_fcn(S)
                # set
                param_value(M[i],M[i]@seq_in)=S
            }
            # use current data
            M[i] = model_apply(M[i],D)
            
            # set the output of this method as the input for the next method
            S = predicted(M[i])
            if (is(S,'DatasetExperiment')) {
                # if its a dataset then update current D
                D = predicted(M[i])
            }
        }
        return(M)
    }
)


