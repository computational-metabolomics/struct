#' Dataset class
#'
#' An object for holding raw data and associated meta data
#'
#' The dataset object is intended to allow raw data and meta data to be kept
#' together, but without needing to store them e.g. in the same data frame. This
#' is conventient for statistical methods where the meta data is often required
#' but not as part of the same matrix as the raw data.
#'
#' @export dataset
#' @slot name Name of the dataset
#' @slot description Brief description of the dataset
#' @slot type The type of dataset e.g. single_block
#' @slot data A data.frame of data, samples in rows, variables/features
#' in columns.
#' @slot sample.meta A data.frame of sample meta data e.g. group
#' membership
#' @slot variable.meta A data frame of variable meta data
#' @param obj,object,x A dataset object
#' @param name The name of the slot to set (data, sample_meta or variable_meta
#' for dataset objects)
#' @param value A data.frame
#' @include generics.R struct_class.R stato_class.R chart_class.R
#' @return The returned value depends on the method used
#' @examples
#' D = dataset()
#'
#' # get the data from a dataset object
#' dataset.data(D) # OR
#' D$data
#'
#' # get the sample meta data from a dataset object
#' dataset.sample_meta(D) # OR
#' D$sample_meta
#'
#' # get the variable meta data from a dataset object
#' dataset.variable_meta(D) # OR
#' D$variable_meta
#'
dataset<-setClass(
    "dataset",
    contains = c("struct_class"),
    slots = c(name = "character",
        description = "character",
        data = "data.frame",
        sample_meta = "data.frame",
        variable_meta = "data.frame"
    ),
    prototype = list(name = "Dataset000",
        description = "an empty dataset object"
    )
    
)

#' @describeIn dataset get the data matrix from a dataset object
#' @export
setMethod(f = "dataset.data",
    signature = c("dataset"),
    definition = function(obj)
    {
        return(obj@data)
    }
)

#' @describeIn dataset get data/sample_meta/variable_meta from a dataset object
#' @export
setMethod(f = "$",
    signature = c("dataset"),
    definition = function(x,name)
    {
        s = c('data','sample_meta','variable_meta')
        if (name %in% s) {
            value = slot(x,name)
            return(value)
        } else {
            stop(paste0('"',name,'" is not a valid slot for dataset objects'))
        }
    }
)

#' @describeIn dataset set the data for a dataset object
#' @export
setMethod(f = "dataset.data<-",
    signature = c("dataset"),
    definition = function(obj,value)
    {
        obj@data = value
        return(obj)
    }
)

#' @describeIn dataset set the data/sample_meta/variable_meta for a dataset
#' @export
setMethod(f = "$<-",
    signature(x = 'dataset'),
    definition = function(x,name,value) {
        s = c('data','sample_meta','variable_meta')
        if (name %in% s) {
            slot(x,name) = value
            return(x)
        } else {
            stop(paste0('"',name,'" is not a valid slot for dataset objects'))
        }
    }
)

#' @describeIn dataset get the sample meta data from a dataset object
#' @export
setMethod(f = "dataset.sample_meta",
    signature = c("dataset"),
    definition = function(obj) {
        return(obj@sample_meta)
    }
)

#' @describeIn dataset set the sample meta data for a dataset
#' @export
setMethod(f = "dataset.sample_meta<-",
    signature = c("dataset"),
    definition = function(obj,value) {
        obj@sample_meta = value
        return(obj)
    }
)

#' @describeIn dataset get the variable meta data from a dataset object
#' @export
setMethod(f = "dataset.variable_meta",
    signature = c("dataset"),
    definition = function(obj)
    {
        return(obj@variable_meta)
    }
)

#' @describeIn dataset set the variable meta data for a dataset
#' @export
setMethod(f = "dataset.variable_meta<-",
    signature = c("dataset"),
    definition = function(obj,value) {
        obj@variable_meta = value
        return(obj)
    }
)

#' @describeIn dataset print a summary of the data set to the terminal
#' @export
#' @import crayon
setMethod(f = "summary",
    signature = c("dataset"),
    definition = function(object) {
        S = list()
        S$name = name(object)
        S$description = description(object)
        S$type = type(object)
        S$n.samples = nrow(dataset.data(object))
        S$n.features = ncol(dataset.data(object))
        S$n.levels = length(levels(dataset.sample_meta(object)[,1]))
        cat(
            bold('A',class(object),'object from the struct package') %+% '\n\n' %+% 
                blue('Name: '),name(object),'\n' %+%
                blue('Description: '),description(object),'\n',
            sep = ''
        )
        cat('\nConsists of ',S$n.samples,' samples and ',S$n.features,
            ' features.\n',sep = '')
        cat('\nThere are ',S$n.levels, ' levels: ',sep = '')
        cat(green(levels(dataset.sample_meta(object)[,1])),sep = ',')
        cat(' in factor named "',green(names(dataset.sample_meta(object))[1]),
            '"',sep = '')
    }
)

#' Export a dataset to an excel file
#' 
#' Exports a dataset object to an excel file with sheets for data, sample_meta and variable_meta
#' @param object a dataset object
#' @param outfile the filename (including path) to write the data to
#' @param transpose TRUE (default) or FALSE to transpose the output data
#' @return an excel file with sheets for data and meta data
#' @rdname export_data
#' @examples
#' \dontrun{
#' D = iris_dataset() # example dataset
#' export.xlsx(D,'iris_dataset.xlsx')
#' }
#' @export
setMethod(f = "export.xlsx",
    signature = c("dataset"),
    definition = function(object,outfile,transpose = TRUE) {
        
        # check for openxlsx
        if (!requireNamespace('openxlsx', quietly = TRUE)) {
            stop('package "openxlsx" was not found. Please install it to use "export.xlsx()".')
        }
        
        
        if (transpose) {
            X = as.data.frame(t(object$data))
        } else {
            X = object$data
        }
        
        OUT = list(
            'data' = X,
            'sample_meta' = object$sample_meta,
            'variable_meta' = object$variable_meta
        )
        openxlsx::write.xlsx(OUT,file = outfile,rowNames = TRUE,colNames = TRUE)
    }
)


#' @export
setMethod(f = 'show',
    signature = c('dataset'),
    definition = function(object) {
        callNextMethod()
        cat('data:          ',nrow(object$data),' rows x ', ncol(object$data),' columns\n',sep='')
        cat('sample_meta:   ',nrow(object$sample_meta),' rows x ', ncol(object$sample_meta),' columns\n',sep='')
        cat('variable_meta: ',nrow(object$sample_meta),' rows x ', ncol(object$sample_meta),' columns\n',sep='')
    }
)