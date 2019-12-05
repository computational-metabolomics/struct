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
    out = .resampler()
    # initialise
    out = .initialize_struct_class(out,...)
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