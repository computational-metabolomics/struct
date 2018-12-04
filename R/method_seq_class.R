#' method.seq class
#'
#' A class for (ordered) lists of methods
#'
#' @param M a method object
#' @param D a dataset object
#' @param x a method.seq object
#' @param i index
#' @param ML a method.seq object
#' @param object a method.seq object
#' @param e1 a method or method.seq object
#' @param e2 a method or method.seq object
#' @param value value
#' @export method.seq
#' @include generics.R parameter_class.R output_class.R struct_class.R
#' @include method_class.R
#' @return a method.seq object
#' @examples
#' MS = method.seq()
#' MS = method() + method()

method.seq<-setClass(
    "method.seq",
    contains = c('struct_class'),
    slots=c(methods='list')
)


#' @describeIn method.seq train the method using input data
#' @export
#' @examples
#' \dontrun{
#' D = dataset()
#' MS = method() + method()
#' MS = method.apply(MS,D)
#' }
#'
setMethod(f="method.apply",
    signature=c("method.seq","dataset"),
    definition=function(M,D)
    {
        # for each method in the list
        S=D # for first in list the input D is the data object
        for (i in seq_len(length(M)))
        {
            # train the method on the output of the previous method
            M[i]=method.apply(M[i],S)
            # set the output of this method as the input for the next method
            S=predicted(M[i])
        }
        return(M)
    }
)

#' @describeIn method.seq get method object by index in sequence
#' @export
#' @examples
#' M = method()
#' MS = method() + method()
#' MS[2]
#'
setMethod(f= "[",
    signature="method.seq",
    definition=function(x,i){
        return(x@methods[[i]])
    }
)

#' @describeIn method.seq set method by index in sequence
#' @export
#' @examples
#' M = method()
#' MS = method() + method()
#' MS[3] = M
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
#' @describeIn method.seq get sequence of methods as list
#' @export
#' @examples
#' MS = method() + method()
#' L = method.steps(MS)
setMethod(f='method.steps',
    signature='method.seq',
    definition=function(ML){
        return(ML@methods)
    }
)

#' @describeIn method.seq set the sequence of methods by inputting a list of
#' methods
#' @export
#' @examples
#' MS = method.seq()
#' L = list(method(),method())
#' L = method.steps(MS) = L
setMethod(f='method.steps<-',
    signature=c('method.seq','list'),
    definition=function(ML,value) {
        # check that all items in list are methods
        ism=lapply(X=value,FUN=is,class2='method')
        if (!all(unlist(ism))) {
            stop('all items in list must be a method')
        }
        # if they are all methods then add them to the object
        ML@methods=value
        return(ML)
    }
)

#' @describeIn method.seq get the length of the method sequence
#' @export
#' @examples
#' MS = method() + method()
#' length(MS) # 2
setMethod(f='length',
    signature='method.seq',
    definition=function(x) {
        return(length(x@methods))
    }
)

#' @describeIn method.seq print a brief summary of a method.seq object
#' @export
#' @examples
#' MS = method() + method()
#' show(MS)
setMethod(f='show',
    signature='method.seq',
    definition=function(object) {
        cat('A method.seq object containing:\n')
        if (length(object)==0)
        {
            cat('no methods')
            return()
        }
        for (i in seq_len(length(object)))
        {
            cat('[',i,'] ',name(object[i]),'\n',sep='')
        }
    }
)

setClassUnion("method_OR_method.seq", c("method", "method.seq"))

#' @describeIn method.seq add a method object to the (front) of a sequence.
#' @export
#' @examples
#' MS = model() + model()
#' M = model()
#' MS = M + MS # method added to front of sequence
setMethod("+",
    signature(e1='method',e2='method.seq'),
    definition=function(e1,e2) {
        m=method.steps(e2)
        m=c(e1,m)
        method.steps(e2)=m
        return(e2)
    }
)

#' @describeIn method.seq add a method object to the (end) of a sequence.
#' @export
#' @examples
#' MS = model() + model()
#' M = model()
#' MS = MS + M # method added to end of sequence
setMethod("+",
    signature(e1='method.seq',e2='method'),
    definition=function(e1,e2) {
        m=method.steps(e1)
        m=c(m,e2)
        method.steps(e1)=m
        return(e1)
    }
)

#' @describeIn method.seq create a method sequence by combining two methods.
#' @export
#' @examples
#' MS = model() + model()
setMethod("+",
    signature(e1='method',e2='method'),
    definition=function(e1,e2) {
        ML=method.seq(methods=c(e1,e2))
        return(ML)
    }
)

