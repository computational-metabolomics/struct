#' Layer Entity objects
#'
#' A class in the \pkg{struct} package for managing ggplot2 layers with preset configurations.
#' Layer entities extend the base entity class to provide functionality for creating
#' and managing ggplot2 layers with predefined presets.
#'
#' @export
#' @param layer Character string naming the ggplot2 layer function (e.g., 'geom_point')
#' @param presets List of preset configurations
#' @param value The value of the layer entity
#' @param ... Additional parameters passed to entity constructor
#' @return A layer_entity object
#' @examples
#' # Create a layer entity
#' le = layer_entity(
#'     layer = 'geom_point',
#'     presets = list(
#'         default = list(color = 'red'),
#'         blue = list(color = 'blue')
#'     ),
#'     value = preset('default')
#' )
#' @rdname layer_entity
#' @include entity_class.R struct_preset_class.R
layer_entity = function(layer = 'geom_point', presets = list(), value = preset('default'), ...) {

    # If value is a character string, convert it to a preset
    if (is.character(value) && length(value) == 1) {
        value = preset(value)
    }

    # NEW: If value is a direct ggplot2 layer object, create a custom preset
    if (is(value, 'Layer') || is(value, 'theme') || is(value, 'Scale') || is(value, 'Facet')) {
        # Create a custom preset with the provided layer
        custom_presets = list(custom = value)
        presets = c(presets, custom_presets)
        # Set the value to use the custom preset
        value = preset('custom')
    }

    # new object
    out = .layer_entity(
        layer = layer,
        presets = presets,
        value = value,
        ...
    )
    return(out)
}

.layer_entity<-setClass(
    "layer_entity",
    contains = 'entity',
    slots = c(
        layer = 'character',
        presets = 'list'
    ),
    prototype = list(
        name = 'ggplot2 layer',
        description = 'A ggplot2 layer e.g. geom_point, stat_ellipse etc',
        type = c('list','struct_preset','NULL'),
        value = list(),
        layer = 'geom_point',
        presets = list(),
        .params = 'layer'
    ),
    validity = function(object) {
        return(TRUE)
    }
)

#' @rdname layer_entity
#' @export
setGeneric("register_preset<-", function(obj, preset_name, value, force = TRUE) standardGeneric("register_preset<-"))

setMethod(f = 'register_preset<-',
    signature = c('layer_entity','character','list'),
    definition = function(obj, preset_name, value, force = TRUE) {

        check = preset_name %in% c(names(obj@presets), 'default')
        if (check && !force) {
            stop('This preset already exists. To replace it use "force = TRUE".')
        }

        obj@presets[[preset_name]] = value
        return(obj)
    }
)

#' @rdname layer_entity
#' @export
setGeneric("available_presets", function(obj, name) standardGeneric("available_presets"))

setMethod(f = 'available_presets',
    signature = c('chart'),
    definition = function(obj, name) {
        return(names(slot(obj, name)@presets))
    }
)

#' @rdname layer_entity
#' @export
setMethod(f = "value<-",
    signature = c("layer_entity"),
    definition = function(obj, value) {

        # If value is a character string, convert it to a preset
        if (is.character(value) && length(value) == 1) {
            value = preset(value)
        }

        # If value is a direct ggplot2 layer object, create a custom preset
        if (is(value, 'Layer') || is(value, 'theme') || is(value, 'Scale') || is(value, 'Facet')) {
            # Create a custom preset with the provided layer
            custom_presets = list(custom = value)
            obj@presets = c(obj@presets, custom_presets)
            # Set the value to use the custom preset
            value = preset('custom')
        }

        if (is(value, 'struct_preset')) {
            # check for valid preset (local only)
            local_check = value$preset %in% c(names(obj@presets))

            if (!local_check) {
                available_local = names(obj@presets)
                stop('"', value$preset, '" is not a valid preset for layer_entity "',
                     obj@layer, '". Choose one of: ', paste(available_local, collapse = ', '))
            }
        }

        # standardise names
        if (is.list(value)) {
            names(value) = ggplot2::standardise_aes_names(names(value))
        }
        obj = callNextMethod(obj, value)

        return(obj)
    }
)

#' @rdname layer_entity
#' @export
setMethod(f = "$<-",
    signature = c("layer_entity"),
    definition = function(x, name, value) {

        # If value is a ggplot2 layer object, store it directly
        if (is(value, 'Layer') || is(value, 'theme') || is(value, 'Scale') || is(value, 'Facet')) {
            name = ggplot2::standardise_aes_names(name)
            x[[name]] = value
            return(x)
        }

        if (is(value, 'struct_preset')) {
            # check for valid preset (local only)
            local_check = value$preset %in% c(names(x@presets))

            if (!local_check) {
                available_local = names(x@presets)
                stop('"', value$preset, '" is not a valid preset for layer_entity "',
                     name, '". Choose one of: ', paste(available_local, collapse = ', '))
            }
        }

        name = ggplot2::standardise_aes_names(name)
        x[[name]] = value

        return(x)
    }
)

#' @rdname layer_entity
#' @export
setMethod(f = 'show',
    signature = c('layer_entity'),
    definition = function(object) {
        callNextMethod() # force the default output
        cat('layer:         ', object@layer, '\n', sep = '')
        cat('available presets:', paste(names(object@presets), collapse = ', '), '\n', sep = '')
    }
)

#' @rdname layer_entity
#' @export
setGeneric("as_layer", function(obj, ...) standardGeneric("as_layer"))

setMethod(f = 'as_layer',
    signature = c('layer_entity'),
    definition = function(obj) {

        # If the value is already a ggplot2 layer object, return it directly
        if (is(obj@value, 'Layer') || is(obj@value, 'theme') || is(obj@value, 'Scale') || is(obj@value, 'Facet')) {
            return(obj@value)
        }

        # If value is NULL, return NULL
        if (is.null(obj@value)) {
            return(NULL)
        }

        # get preset
        check = any(names(obj@value) == "preset")
        if (check) {
            # get preset
            P = get_preset(obj, obj@value$preset)
            # remove preset label
            obj@value$preset = NULL

            if (!is.null(P)) {
                # If P is already a ggplot2 layer object, return it directly
                if (is(P, 'Layer') || is(P, 'theme') || is(P, 'Scale') || is(P, 'Facet')) {
                    return(P)
                }
                # Otherwise, update preset with overrides
                obj@value = modifyList(P, obj@value)
            } else {
                obj@value = NULL
            }
        }

        # return NULL if specified; this layer not to be plotted
        if (is.null(obj@value)) {
            return(NULL)
        }

        if (length(obj@value) > 0) {

            L = obj@value

            # get mappings
            z = which(unlist(lapply(L, is, class2 = 'uneval')))
            # join all mappings
            mappings = list(mapping = Reduce(.modify_aes, L[z]))
            # params without mappings
            L = L[-z]
        } else {
            mappings = list(NULL)
            L = list(NULL)
        }
        # construct layer
        g = do.call(obj@layer, c(mappings, L))
        return(g)
    }
)

#' @rdname layer_entity
#' @export
setMethod(f = "value",
    signature = c("layer_entity"),
    definition = function(obj) {
        return(obj@value)
    }
)

#' @rdname layer_entity
#' @export
setMethod(f = "get_preset",
    signature = c('layer_entity','character','missing'),
    definition = function(obj, preset_name, slot_name) {

        # Check local presets only
        check = preset_name %in% c(names(obj@presets))
        if (check) {
            return(obj@presets[[preset_name]])
        }

        # If we get here, the preset doesn't exist
        available_local = names(obj@presets)

        stop('"', preset_name, '" is not a valid preset for layer_entity "',
             obj@layer, '". Choose one of: ', paste(available_local, collapse = ', '))
    }
)

# Helper function to modify aesthetics (simplified version)
.modify_aes = function(a, b) {
    # This is a simplified version - in practice, this would properly merge ggplot2 aesthetics
    # For now, just return the first argument
    return(a)
}
