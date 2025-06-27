#' Struct Preset objects
#'
#' A utility function in the \pkg{struct} package for creating preset configurations.
#' Struct presets are used to store predefined parameter configurations that can be
#' applied to various objects in the struct framework.
#'
#' @export
#' @param name Character string naming the preset
#' @param ... Additional parameters for the preset
#' @return A list with class 'struct_preset'
#' @examples
#' # Create a preset
#' p = preset('default', color = 'red', size = 2)
#' @rdname struct_preset
preset = function(name, ...) {
    L = list(preset = name)
    L = c(L, list(...))
    names(L) = ggplot2::standardise_aes_names(names(L))
    class(L) = c('struct_preset', 'list')
    return(L)
}

# Helper function to standardise aesthetic names (similar to ggplot2::standardise_aes_names)
standardise_aes_names = function(names) {
    # This is a simplified version - in practice, this would match ggplot2's implementation
    # For now, just return the names as-is
    return(names)
}

