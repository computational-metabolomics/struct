#' resampler class
#'
#' A class for resampling methods such as cross-validation. not intended to be
#' called directly.
#' @include generics.R iterator_class.R
#' @export resampler
#' @examples
#' R = resampler()

resampler<-setClass(
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