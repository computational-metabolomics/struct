#' StRUCT templates
#'
#' Create a struct template
#' @export
#' @param template the type of object you want a template for e.g. 'model'
#' @param output the name/path of the output file
#' @param in_editor TRUE/FALSE to open the created file in the default editor
#' @param overwrite = TRUE/FALSE to overwrite file if exists already
#' @return A template is created at the output location specified
#' @importFrom utils file.edit
#' @importFrom knitr purl
#' @examples
#'\dontrun{
#' struct_template('model','example.R',FALSE)
#'}
#'
struct_template=function(
    template='model',
    output,
    in_editor = TRUE,
    overwrite = FALSE
) {

    if ((!template %in% c('model'))) {
        return(stop('Incorrect template'))
    }

    fn=file.path(path.package('struct'),
        'doc',paste0('struct_',template,'.Rmd'))

    if (file.exists(output) & !overwrite) {
        stop('Output file already exists. Use overwrite = TRUE if you want to
            replace the existing file.')
    }

    knitr::purl(input = fn,
        documentation = 0,quiet = TRUE,
        output=output                   # name/path of the R script to create
    )

    if (in_editor) {
        # use rstudio if available
        if (rstudioapi::isAvailable()) {
            rstudioapi::navigateToFile(output)
        } else { # otherwise default to the internal editor
            utils::file.edit(output,editor='internal')
        }
    }
}
