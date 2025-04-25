#' @include generics.R struct_class.R parameter_class.R output_class.R

# Define the entity_constraint class
setClass("entity_constraint",
         contains = 'struct_class',
         slots = c(
             message = "function",
             test = "function"))

# Define the generic function for validation
setGeneric("check_constraints", function(entity_constraint, object) {
    standardGeneric("check_constraints")
})

# Define the method for the entity_constraint class
setMethod("check_constraints", "entity_constraint", function(entity_constraint, object) {
    # check validity
    valid = entity_constraint@test(object)

    # return error msg if not valid
    if (!valid) {
        valid = entity_constraint@message(object)
    }

    # return TRUE if valid, otherwise returns message
    return(valid)

})

# Define specific entity_constraints as instances of the entity_constraint class
constraint.max_len <- function() {
    new_struct("entity_constraint",
        name = 'Maximum length constraint',
        description = paste0('Checks that the value of an entity object not longer than specified.'),
        message = function(object){
            msg = c(paste0('entity$name = ',object$name),
                    paste0('entity$value must be at least ', min_value))
            return(msg)
        },
        test = function(object) {
            check = length(object@value) <= object@max_length
            return(check)
        })
}

constraint.type <- function() {
    new_struct("entity_constraint",
        name = 'Type constraint',
        description = 'Checks that the value of an entity object is of the expected type.',
        message = function(object){
            msg = c(paste0('entity$name = ',object$name),
                    paste0('entity$value must be of type ', object$type))
            return(msg)
        },
        test = function(object) {
            check = any(sapply(object@type, function(t) is(object@value, t)))
            return(check)
        })
}


# test for minimum
constraint.min_value <- function(min_value) {
    new_struct("entity_constraint",
        name = 'Minimum value constraint',
        description = paste0('Checks that the value of an entity object not less than ',min_value),
        message = paste("Minimum value must be at least", min_value),
        test = function(object) {
            if (any(object@value < min_value)) {
                msg = c(paste0('entity$name = ',object$name),
                        paste0('entity$value must be at least ', min_value))
                return(msg)
            }
            return(TRUE)
        })
}
