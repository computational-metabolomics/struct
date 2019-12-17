#' DatasetExperiment class
#'
#' An object for holding raw data and associated meta data
#'
#' The DatasetExperiment object is an extension of the SummarizedExperiment object 
#' from the SummarizedExperiment package (found on Bioconductor). 
#' It incorporates the basic functionality of struct objects, containing fields such as 
#' Description, Name and Type with features of SummarizedExperiment such as subsetting.
#' 
#' There are some important differences between DatasetExperiment and SummarizedExperiment:
#' \itemize{
#' \item In DatasetExperiment data is stored as Samples (rows) x Features (columns)
#' \item DatasetExperiment currently only supports a single assay
#' \item length(DatasetExperiment) returns the number of samples
#' }
#'  
#' @export
#' @slot name Name of the dataset
#' @slot description Brief description of the dataset
#' @slot type The type of dataset e.g. single_block
#' @param x A DatasetExperiment object
#' @param ... slots and values to populate a new DatasetExperiment with
#' @param name DatasetExperiment slot to get/set
#' @param value the value to assign to the named slot
#' @import SummarizedExperiment
#' @import S4Vectors
#' @include generics.R struct_class.R stato_class.R chart_class.R
#' @return DatasetExperiment
#' @rdname struct_DatasetExperiment
DatasetExperiment = function(...) {
    
    L = list(...)
    
    # change slot names to match summarizedExperiment
    if ('data' %in% names(L)) {
        w = which(names(L) == 'data')
        names(L)[w] = 'assay'
    }
    if ('sample_meta' %in% names(L)) {
        w = which(names(L) == 'sample_meta')
        names(L)[w] = 'rowData'
    }
    if ('variable_meta' %in% names(L)) {
        w = which(names(L) == 'variable_meta')
        names(L)[w] = 'colData'
    }
    w=which(names(L) %in% c('assay','rowData','colData'))
    
    # use SummarizedExperiment
    # this way we get athe relevant error if row or col data are set without assay
    if (any(c('data','assay') %in% names(L))) {
        out = .DatasetExperiment(
            SummarizedExperiment(
                assays=L$assay,
                rowData=L$rowData,
                colData=L$colData)
        )
    } else {
        out = .DatasetExperiment(
            SummarizedExperiment()
        )

    }
    
    # remove SummarizedExperiment slots
    L=L[-w]
    
    # populate remaining slots
    if (length(L) > 0) {
        for (k in names(L)) {
            slot(out,k)=L[[k]]
        }
    }
    
    return(out)
}

.DatasetExperiment <- setClass(
    "DatasetExperiment", 
    contains = c("struct_class","SummarizedExperiment")
)

#' @rdname struct_DatasetExperiment
#' @export
setMethod(f = "$",
    signature = c("DatasetExperiment"),
    definition = function(x,name) {
        
        s = c('data','sample_meta','variable_meta')
        
        if (name %in% s) {
            if (name %in% c('data')) {
                if (length(assays(x))==0) {
                    value=NULL
                } else {
                    value = assay(x,1)
                }
            } else if (name %in% c('sample_meta')) {
                value = rowData(x)
            } else if (name %in% c('variable_meta')) {
                value = S4Vectors::DataFrame(colData(x)) # because it returns a DFrame for some reason
            } 
            
            if (name %in% c('data','sample_meta','variable_meta')) {
                # convert to data.frame if using the original struct definitions
                value=as.data.frame(value)
            }
            
            return(value)
            
        } else {
            return(callNextMethod())
        }
        
    }
)

#' @rdname struct_DatasetExperiment
#' @export
setMethod(f = "$<-",
    signature(x = 'DatasetExperiment'),
    definition = function(x,name,value) {
        s = c('data','sample_meta','variable_meta')
        if (name %in% s) {
            if (name %in% c('data')) {
                assay(x,1) = value
            } else if (name %in% c('sample_meta')) {
                rowData(x) = value
            } else if (name %in% c('sample_meta')) {
                colData(x) = value
            }
            return(x)
        } else {
            callNextMethod()
        }
    }
)


setMethod(f = 'show',
    signature = c('DatasetExperiment'),
    definition = function(object) {
        
        # print struct generic info
        callNextMethod()
        
        # number of assays
        nms <- length(assays(object))
        if (is.null(nms)) {
            # if null then no assays yet
            cat('data:          0 rows x 0 columns\n',sep='')
        } else {
            cat('data:          ',nrow(object$data),' rows x ', ncol(object$data),' columns\n',sep='')
        }
        cat('sample_meta:   ',nrow(object$sample_meta),' rows x ', ncol(object$sample_meta),' columns\n',sep='')
        cat('variable_meta: ',nrow(object$variable_meta),' rows x ', ncol(object$variable_meta),' columns\n',sep='')
    }
)

#' Convert a DatasetExperiment to SummarizedExperiment
#' 
#' Converts a DatasetExperiment to SummarizedExperiment. The assay data is 
#' transposed, and colData and rowData switched to match. struct specific
#' slots such as "name" and "description" are stored in the metaData.
#' @param obj a DatasetExperiment object
#' @return a SummarizedExperiment object
#' @export
setMethod (f = 'as.SummarizedExperiment',
    signature = 'DatasetExperiment',
    definition = function(obj) {
        out=SummarizedExperiment(
            assays=list(t(obj$assay)),
            colData=rowData(obj),
            rowData=colData(obj),
            metadata=list(
                'name'=obj$name,
                'description'=obj$description,
                'type'=obj$type,
                'libraries'=obj$libraries)
        )
        
        return(out)
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
#' D = iris_DatasetExperiment() # example dataset
#' export.xlsx(D,'iris_DatasetExperiment_xlsx')
#' }
#' @export
setMethod(f = "export_xlsx",
    signature = c("DatasetExperiment"),
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
