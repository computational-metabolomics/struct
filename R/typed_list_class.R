#' Typed List objects
#'
#' A utility class in the \pkg{struct} package for creating typed lists with metadata.
#' Typed lists are used to store collections of objects with a common type annotation.
#'
#' @export
#' @param ... List elements to include
#' @param .type Character string describing the type of list
#' @param drop_null Logical, whether to remove NULL elements from the list
#' @return A typed list object
#' @examples
#' # Create a typed list
#' tl = typed_list(
#'     point1 = geom_point(),
#'     point2 = geom_line(),
#'     .type = 'Layer',
#'     drop_null = TRUE
#' )
#' @rdname typed_list
typed_list = function(..., .type = NULL, drop_null = FALSE) {
    
    # Get all arguments
    args = list(...)
    
    # Remove NULL elements if requested
    if (drop_null) {
        args = args[!sapply(args, is.null)]
    }
    
    # Infer type from first element if not provided
    if (is.null(.type) && length(args) > 0) {
        first_element = args[[1]]
        .type = class(first_element)[1]
        }
    
    
    # Create the typed list object
    out = .typed_list(
        elements = args,
        type = .type
    )
    
    return(out)
}

.typed_list<-setClass(
    "typed_list",
    slots = c(
        elements = 'list',
        type = 'character'
    ),
    prototype = list(
        elements = list(),
        type = character(0)
    ),
    validity = function(object) {
        # Basic validation
        if (length(object@type) > 1) {
            return("type must be a single character string or NULL")
        }
        return(TRUE)
    }
)

#' @rdname typed_list
#' @export
setMethod(f = "show",
    signature = c("typed_list"),
    definition = function(object) {
        cat("Typed list")
        if (length(object@type) > 0) {
            cat(" (", object@type, ")", sep = "")
        }
        cat(" with", length(object@elements), "elements\n")
        
        if (length(object@elements) > 0) {
            for (i in seq_along(object@elements)) {
                name = names(object@elements)[i]
                if (is.null(name) || name == "") {
                    name = paste0("[[", i, "]]")
                }
                cat("  ", name, ": ", class(object@elements[[i]])[1], "\n", sep = "")
            }
        }
    }
)

#' @rdname typed_list
#' @export
setMethod(f = "length",
    signature = c("typed_list"),
    definition = function(x) {
        return(length(x@elements))
    }
)

#' @rdname typed_list
#' @export
setMethod(f = "names",
    signature = c("typed_list"),
    definition = function(x) {
        return(names(x@elements))
    }
)

#' @rdname typed_list
#' @export
setMethod(f = "names<-",
    signature = c("typed_list", "character"),
    definition = function(x, value) {
        names(x@elements) = value
        return(x)
    }
)

#' @rdname typed_list
#' @export
setMethod(f = "[[",
    signature = c("typed_list", "ANY"),
    definition = function(x, i, j, ...) {
        return(x@elements[[i, ...]])
    }
)

#' @rdname typed_list
#' @export
setMethod(f = "[[<-",
    signature = c("typed_list", "ANY", "missing", "ANY"),
    definition = function(x, i, j, value) {
        x@elements[[i]] = value
        return(x)
    }
)

#' @rdname typed_list
#' @export
setMethod(f = "[",
    signature = c("typed_list", "ANY"),
    definition = function(x, i, j, ...) {
        new_elements = x@elements[i, ...]
        return(.typed_list(elements = new_elements, type = x@type))
    }
)

#' @rdname typed_list
#' @export
setMethod(f = "as.list",
    signature = c("typed_list"),
    definition = function(x, ...) {
        return(x@elements)
    }
) 