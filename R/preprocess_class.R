#' preprocessing class
#'
#' A class used for preprocessing steps that require application to test sets.
#' not intended to be called directly, this class should be inherited to provide
#' functionality for method-specific classes.
#' @export preprocess
#' @param M a model object
#' @param D a dataset object
#' @include generics.R model_class.R
#' @examples
#' M = preprocess()

preprocess<-setClass(
    "preprocess",
    contains = c('model')
)

#' @describeIn preprocess reverse the preprocessing step applied. Used in cases
#' such as mean centring, where you may want to 'uncentre' the results
#' @export
#' @examples
#' D = dataset()
#' M = model()
#' D2 = model_reverse(M,D)
#' @return dataset object
setMethod(f = "model_reverse",
    signature = c("preprocess","dataset"),
    definition = function(M,D) {
        warning('no reverse method implemented for this model')
        return(D)
    }
)

#' @export
setMethod(f = "show",
    signature = c("preprocess"),
    definition = function(object) {
        callNextMethod()
    }
)

