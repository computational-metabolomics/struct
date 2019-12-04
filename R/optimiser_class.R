#' optimiser class
#'
#' A special class of iterator for selecting optimal parameter values
#' not intended to be called directly, this class should be inherited to provide
#' functionality for method-specific classes.
#' @export optimiser
#' @include generics.R    parameter_class.R output_class.R model_class.R
#' @include iterator_class.R
#' @examples
#' OPT = optimiser()

optimiser<-setClass(
    "optimiser",
    contains = c('iterator'),
    slots = c(type = 'character',
        outputs.optimal_model = 'model_OR_iterator'
    )
)

setClassUnion("model_OR_optimiser", c("model", "optimiser"))
