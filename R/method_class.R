#' method class
#'
#' A class for methods that are applied to datasets in a single step e.g.
#' normalisation. Not intended to be called directly, this class should be
#' inherited to provide functionality for method-specific classes.
#' @export method
#' @include generics.R    parameter_class.R output_class.R dataset_class.R
#' @param M a model object
#' @param D a dataset object
#' @param value value
#' @examples
#' M = method() # create a method object
#'
method<-setClass(
    "method",
    contains = c('struct_class','parameter_class','outputs_class'),
    slots=c(type='character',
        predicted='character'
    )
)

#' @describeIn method train the model using input data
#' @export
#' @examples
#' D = iris_dataset() # example dataset
#' M = method.apply(M,D) # apply method to data
#'
setMethod(f="method.apply",
    signature=c("method","dataset"),
    definition=function(M,D)
    {
        warning('no method implemented yet')
        return(M)
    }
)

#' @describeIn method get prediction output from model
#' @export
#' @examples
#' \dontrun{
#'
#' p = predicted(M) # get the predicted output from the model
#' }
setMethod(f='predicted',
    signature=c('method'),
    definition=function(M)
    {
        return(output.value(M,predicted.name(M)))
    }
)

#' @describeIn method get prediction output name for model
#' @export
#' @examples
#'
#' \dontrun{
#'
#' M = method()
#' predicted.name(M)
#' }
#'
setMethod(f='predicted.name',
    signature=c('method'),
    definition=function(M)
    {
        return(M@predicted)
    }
)

#' @describeIn method set prediction output from method
#' @export
#' @examples
#' M = method()
#' predicted.name(M) = 'example'
#'
setMethod(f='predicted.name<-',
    signature=c('method','character'),
    definition=function(M,value)
    {
        M@predicted=value
        return(M)
    }
)


