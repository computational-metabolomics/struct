#' model.seq class
#'
#' A class for (ordered) lists of models
#'
#' @export model.seq
#' @param M a model object
#' @param D a dataset object
#' @param x a model.seq object
#' @param i index
#' @param ML a model.seq object
#' @param object a model.seq object
#' @param e1 a model or model.seq object
#' @param e2 a model or model.seq object
#' @param value value
#' @include generics.R    parameter_class.R output_class.R struct_class.R model_class.R model_stato_class.R
#' @examples
#' MS = model.seq()
#' MS = model() + model()

model.seq<-setClass(
    "model.seq",
    contains = c('struct_class'),
    slots=c(models='list')
)


#' @describeIn model.seq train the model using input data
#' @export
#' @examples
#' \dontrun{
#' MS = model() + model()
#' MS = model.train(MS)
#' }
#' @return model sequence
setMethod(f="model.train",
    signature=c("model.seq","dataset"),
    definition=function(M,D)
    {
        # for each model in the list
        S=D # for first in list the input D is the data object
        for (i in 1:length(M))
        {
            M[i]=model.train(M[i],S) # train the model on the output of the previous model
            M[i]=model.predict(M[i],S) # apply the model to the output of the previous model
            S=predicted(M[i]) # set the output of this model as the input for the next model
        }
        return(M)
    }
)

#' @describeIn model.seq apply the model to input data
#' @export
#' @examples
#' \dontrun{
#' D= dataset()
#' MS = model() + model()
#' MS = model.train(MS)
#' MS = model.predict(MS,D)
#' }
#' @return model sequence
setMethod(f="model.predict",
    signature=c("model.seq",'dataset'),
    definition=function(M,D)
    {
        S=D # for the first model the input use the input data
        for (i in 1:length(M))
        {
            M[i]=model.predict(M[i],S) # apply the model the output of the previous model
            S=predicted(M[i]) # set the output of this model as the input to the next
        }
        return(M)
    }
)

#' @describeIn model.seq get model by index
#' @export
#' @examples
#'
#' MS = model() + model()
#' MS[2]
#'
#' @return model at the given index in the sequence
setMethod(f= "[",
    signature="model.seq",
    definition=function(x,i){
        return(x@models[[i]])
    }
)

#' @describeIn model.seq set model by index
#' @export
#' @examples
#'
#' MS = model() + model()
#' MS[3] = model()
#'
#' @return model sequence with the model at index i replaced
setMethod(f= "[<-",
    signature="model.seq",
    definition=function(x,i,value){
        if (!is(value,'model'))
        {
            stop('value must be a model')
        }
        x@models[[i]]=value
        return(x)
    }
)

#' @describeIn model.seq get a list of models in the sequence
#' @export
#' @examples
#'
#' MS = model() + model()
#' L = models(MS)
#'
#' @return a list of models in the sequence
setMethod(f='models',
    signature='model.seq',
    definition=function(ML){
        return(ML@models)
    }
)

#' @describeIn model.seq set the models in the sequence
#' @export
#' @examples
#'
#' MS = model.seq()
#' L = list(model(),model())
#' models(MS) = L
#'
#' @return a model sequence containing the input models
setMethod(f='models<-',
    signature=c('model.seq','list'),
    definition=function(ML,value) {
        # check that all items in list are models
        ism=lapply(X=value,FUN=is,class2='model')
        if (!all(unlist(ism))) {
            stop('all items in list must be a model')
        }
        # if they are all models then add them to the object
        ML@models=value
        return(ML)
    }
)

#' @describeIn model.seq get the number of models in the sequence
#' @export
#' @examples
#'
#' MS = model() + model()
#' length(MS) # 2
#'
#' @return the number of models in the sequence
setMethod(f='length',
    signature='model.seq',
    definition=function(x) {
        return(length(x@models))
    }
)

#' @describeIn model.seq print a summary of the model.seq to the command line
#' @export
#' @examples
#'
#' MS = model() + model()
#' show(MS)
#'
#' @return pritns a summary of the contents of a model sequence
setMethod(f='show',
    signature='model.seq',
    definition=function(object) {
        cat('A model.seq object containing:\n')
        if (length(object)==0)
        {
            cat('no models')
            return()
        }
        for (i in 1:length(object))
        {
            cat('[',i,'] ',name(object[i]),'\n',sep='')
        }
    }
)

setClassUnion("model_OR_model.seq", c("model", "model.seq","model.stato"))

#' @describeIn model.seq add a model to the (front) of a model sequence
#' @export
#' @examples
#'
#' MS = model() + model()
#' M = model()
#' MS = M + MS
#'
#' @return a model sequence with the additional model appended to the front of the sequence
setMethod("+",
    signature(e1='model',e2='model.seq'),
    definition=function(e1,e2) {
        m=models(e2)
        m=c(e1,m)
        models(e2)=m
        return(e2)
    }
)

#' @describeIn model.seq add a model to the (end) of a model sequence
#' @export
#' @examples
#'
#' MS = model() + model()
#' M = model()
#' MS = MS + M
#'
#' @return a model sequence with the additional model appended to the end of the sequence
setMethod("+",
    signature(e1='model.seq',e2='model'),
    definition=function(e1,e2) {
        m=models(e1)
        m=c(m,e2)
        models(e1)=m
        return(e1)
    }
)

#' @describeIn model.seq combine two models into a sequence
#' @export
#' @examples
#'
#' MS = model() + model()
#'
#' @return a model sequence
setMethod("+",
    signature(e1='model',e2='model'),
    definition=function(e1,e2) {
        ML=model.seq(models=c(e1,e2))
        return(ML)
    }
)

