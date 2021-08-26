#' optimiser class
#'
#' A special class of iterator for selecting optimal parameter values
#' not intended to be called directly, this class should be inherited to provide
#' functionality for method-specific classes.
#' @export optimiser
#' @include generics.R parameter_class.R output_class.R model_class.R
#' @include iterator_class.R
#' @examples
#' OPT = optimiser()
#' @param ... named slots and their values.
#' @rdname optimiser
#' @return an optimiser object
optimiser = function(...) {
    # new object
    out = new_struct('optimiser',...)
    return(out)
}

.optimiser<-setClass(
    "optimiser",
    contains = c('iterator'),
    slots = c(type = 'character',
        outputs_optimal_model = 'model_OR_iterator'
    )
)

setMethod(f = "show",
    signature = c("optimiser"),
    definition = function(object) {
        callNextMethod()
    }
)

# autocompletion, return sample_meta column names
#' @export
#' @method .DollarNames optimiser
#' @rdname autocompletion
.DollarNames.optimiser<- function(x, pattern = "") {
    .DollarNames.struct_class(x,pattern)
}

#' @export 
#' @rdname autocompletion
setMethod('.DollarNames','optimiser',.DollarNames.optimiser)

