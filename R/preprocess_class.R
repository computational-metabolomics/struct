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
#' @param ... named slots and their values.
#' @rdname preprocessing
preprocess = function(...) {
    # new object
    out = .preprocess()
    # initialise
    out = .initialize_struct_class(out,...)
    return(out)
}

.preprocess<-setClass(
    "preprocess",
    contains = c('model')
)

#' @rdname preprocessing
#' @export
#' @examples
#' D = DatasetExperiment()
#' M = model()
#' D2 = model_reverse(M,D)
#' @return dataset object
setMethod(f = "model_reverse",
    signature = c("preprocess","DatasetExperiment"),
    definition = function(M,D) {
        warning('no reverse method implemented for this model')
        return(D)
    }
)


setMethod(f = "show",
    signature = c("preprocess"),
    definition = function(object) {
        callNextMethod()
    }
)

