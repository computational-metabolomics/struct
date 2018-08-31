#' model.list class
#'
#' A class for (ordered) lists of models
#'
#' @export model.list
#' @include generics.R  parameter_class.R output_class.R struct_class.R model_class.R model_stato_class.R

model.list<-setClass(
  "model.list",
  contains = c('struct_class'),
  slots=c(models='list')
)


#' @describeIn model.list train the model using input data
#' @export
setMethod(f="model.train",
          signature=c("model.list","dataset"),
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

#' @describeIn model.list apply the model to input data
#' @export
setMethod(f="model.predict",
          signature=c("model.list",'dataset'),
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
  signature="model.list",
  definition=function(x,i){
    return(x@models[[i]])
  }
)

#' @export
setMethod(f= "[<-",
          signature="model.list",
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
          signature='model.list',
          definition=function(ML){
            return(ML@models)
          }
)

#' @export
setMethod(f='models<-',
          signature=c('model.list','list'),
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
          signature='model.list',
          definition=function(x) {
            return(length(x@models))
          }
)

#' @export
setMethod(f='show',
          signature='model.list',
          definition=function(object) {
            cat('A model.list object containing:\n')
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

setClassUnion("model_OR_model.list", c("model", "model.list","model.stato"))

#' @export
setMethod(f="param.value<-",
          signature=c("model.list","character",'numeric'),
          definition=function(obj,name,idx,value)
          {
             param.value(obj[idx],name)=value
             return(obj)
          }
)
