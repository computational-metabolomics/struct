#' model.seq class
#'
#' A class for (ordered) lists of models
#'
#' @export model.seq
#' @include generics.R  parameter_class.R output_class.R struct_class.R model_class.R model_stato_class.R

model.seq<-setClass(
  "model.seq",
  contains = c('struct_class'),
  slots=c(models='list')
)


#' @describeIn model.seq train the model using input data
#' @export
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

#' @export
setMethod(f= "[",
  signature="model.seq",
  definition=function(x,i){
    return(x@models[[i]])
  }
)

#' @export
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

#' @export
setMethod(f='models',
          signature='model.seq',
          definition=function(ML){
            return(ML@models)
          }
)

#' @export
setMethod(f='models<-',
          signature=c('model.seq','list'),
          definition=function(ML,value) {
            # check that all items in list are models
            ism=lapply(X=value,FUN=isClass,Class='model')
            if (!all(unlist(ism))) {
              stop('all items in list must be a model')
            }
            # if they are all models then add them to the object
            ML@models=value
            return(ML)
          }
)

#' @export
setMethod(f='length',
          signature='model.seq',
          definition=function(x) {
            return(length(x@models))
          }
)

#' @export
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

#' @export
setMethod(f="param.value<-",
          signature=c("model.seq","character",'numeric'),
          definition=function(obj,name,idx,value)
          {
             param.value(obj[idx],name)=value
             return(obj)
          }
)

#' @export
setMethod("+",
          signature(e1='model',e2='model.seq'),
          definition=function(e1,e2) {
            m=models(e2)
            m=c(e1,m)
            models(e2)=m
            return(e2)
          }
)

#' @export
setMethod("+",
          signature(e1='model.seq',e2='model'),
          definition=function(e1,e2) {
            m=models(e1)
            m=c(m,e2)
            models(e1)=m
            return(e1)
          }
)


