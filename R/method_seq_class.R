#' method.seq class
#'
#' A class for (ordered) lists of methods
#'
#' @export method.seq
#' @include generics.R  parameter_class.R output_class.R struct_class.R method_class.R method_stato_class.R

method.seq<-setClass(
  "method.seq",
  contains = c('struct_class'),
  slots=c(methods='list')
)


#' @describeIn method.seq train the method using input data
#' @export
setMethod(f="method.apply",
          signature=c("method.seq","dataset"),
          definition=function(M,D)
          {
            # for each method in the list
            S=D # for first in list the input D is the data object
            for (i in 1:length(M))
            {
              M[i]=method.apply(M[i],S) # train the method on the output of the previous method
              S=predicted(M[i]) # set the output of this method as the input for the next method
            }
            return(M)
          }
)


#' @export
setMethod(f= "[",
          signature="method.seq",
          definition=function(x,i){
            return(x@methods[[i]])
          }
)

#' @export
setMethod(f= "[<-",
          signature="method.seq",
          definition=function(x,i,value){
            if (!is(value,'method'))
            {
              stop('value must be a method')
            }
            x@methods[[i]]=value
            return(x)
          }
)

#' @export
setMethod(f='method.steps',
          signature='method.seq',
          definition=function(ML){
            return(ML@methods)
          }
)

#' @export
setMethod(f='method.steps<-',
          signature=c('method.seq','list'),
          definition=function(ML,value) {
            # check that all items in list are methods
            ism=lapply(X=value,FUN=isClass,Class='method')
            if (!all(unlist(ism))) {
              stop('all items in list must be a method')
            }
            # if they are all methods then add them to the object
            ML@methods=value
            return(ML)
          }
)

#' @export
setMethod(f='length',
          signature='method.seq',
          definition=function(x) {
            return(length(x@methods))
          }
)

#' @export
setMethod(f='show',
          signature='method.seq',
          definition=function(object) {
            cat('A method.seq object containing:\n')
            if (length(object)==0)
            {
              cat('no methods')
              return()
            }
            for (i in 1:length(object))
            {
              cat('[',i,'] ',name(object[i]),'\n',sep='')
            }
          }
)

setClassUnion("method_OR_method.seq", c("method", "method.seq","method.stato"))

#' @export
setMethod(f="param.value<-",
          signature=c("method.seq","character",'numeric'),
          definition=function(obj,name,idx,value)
          {
            param.value(obj[idx],name)=value
            return(obj)
          }
)

#' @export
setMethod("+",
          signature(e1='method',e2='method.seq'),
          definition=function(e1,e2) {
            m=method.steps(e2)
            m=c(e1,m)
            method.steps(e2)=m
            return(e2)
          }
)

#' @export
setMethod("+",
          signature(e1='method.seq',e2='method'),
          definition=function(e1,e2) {
            m=method.steps(e1)
            m=c(m,e2)
            method.steps(e1)=m
            return(e1)
          }
)

#' @export
setMethod("+",
          signature(e1='method',e2='method'),
          definition=function(e1,e2) {
            ML=method.seq(methods=c(e1,e2))
            return(ML)
          }
)

