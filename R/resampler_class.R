#' resampler class
#'
#' A class for resampling methods such as cross-validation. not intended to be
#' called directly.
#' @include generics.R iterator_class.R
#' @export resampler
#' @examples
#' R = resampler()
#' @param ... named slots and their values.
#' @rdname resampler
#' @return a resampler object
resampler = function(...) {
    # new object
    out = .resampler(...)
    return(out)
}

.resampler<-setClass(
    "resampler",
    contains = c('iterator'),
    slots = c(models = 'model_OR_iterator')
)



setMethod(f = "show",
    signature = c("optimiser"),
    definition = function(object) {
        callNextMethod()
    }
)

# autocompletion, return sample_meta column names
#' @export
#' @rdname autocompletion
#' @method .DollarNames resampler
.DollarNames.resampler<- function(x, pattern = "") {
    .DollarNames.struct_class(x,pattern)
}

#' @export 
#' @rdname autocompletion
setMethod('.DollarNames','resampler',.DollarNames.resampler)