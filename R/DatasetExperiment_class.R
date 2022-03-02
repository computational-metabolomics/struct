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
#' @param data A data frame with samples in rows and features in columns
#' @param sample_meta A data frame with samples in rows and meta data in columns
#' @param variable_meta A data frame with features in rows and meta data in columns
#' @param name DatasetExperiment slot to get/set
#' @param value the value to assign to the named slot
#' @param ... named slot values to pass through to struct_class
#' @import SummarizedExperiment
#' @import S4Vectors
#' @include generics.R struct_class.R stato_class.R chart_class.R
#' @return DatasetExperiment
#' @rdname struct_DatasetExperiment
DatasetExperiment = function(
  data=data.frame(),
  sample_meta=data.frame(),
  variable_meta=data.frame(),
  ...){
  
  # convert data set to list
  assays=list(data)
  
  # sample_meta
  
  out=.DatasetExperiment(SummarizedExperiment(
    assays=assays,
    colData=variable_meta,
    rowData=sample_meta),
    ...)
  
  return(out)
}

.DatasetExperiment <- setClass(
  "DatasetExperiment", 
  contains = c("struct_class","SummarizedExperiment"),
  prototype=list('libraries'='SummarizedExperiment')
)

#' @rdname struct_DatasetExperiment
#' @export
setMethod(f = "$",
  signature = c("DatasetExperiment"),
  definition = function(x,name) {
    
    s = c('data','sample_meta','variable_meta')
    
    if (name %in% s) {
      if (name == 'data') {
        if (length(assays(x))==0) {
          value=NULL
        } else {
          value = assay(x,1)
        }
      } else if (name == 'sample_meta') {
        value = S4Vectors::DataFrame(rowData(x)) 
      } else if (name == 'variable_meta') {
        value = S4Vectors::DataFrame(colData(x))
      } 
      
      if (name %in% s) {
        # convert to data.frame if using the original struct definitions
        value=as.data.frame(value)
      }
      
      return(value)
      
    } else {
      # for name,description etc
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
        rowData(x) = S4Vectors::DataFrame(value)
      } else if (name %in% c('variable_meta')) {
        colData(x) = S4Vectors::DataFrame(value)
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
      assays=list(t(obj$data)),
      colData=SummarizedExperiment::rowData(obj),
      rowData=SummarizedExperiment::colData(obj),
      metadata=list(
        'name'=obj$name,
        'description'=obj$description,
        'type'=obj$type,
        'libraries'=obj$libraries)
    )
    
    return(out)
  }
)


#' Convert a SummarizedExperiment to DatasetExperiment
#' 
#' The assay data is transposed, and colData and rowData switched to match. 
#' struct specific slots such as "name" and "description" are extracted from the 
#' metaData if available. NB Any additional metadata will be lost during this conversion.
#' @param obj a SummarizedExperiment object
#' @return a DatasetExperiment object
#' @export
setMethod (f = 'as.DatasetExperiment',
  signature = 'SummarizedExperiment',
  definition = function(obj) {

    A = assay(obj)
    B = as.data.frame(t(A))
    colnames(B) = rownames(A)
    rownames(B) = colnames(A)
    
    out=DatasetExperiment(
      data=B,
      variable_meta=as.data.frame(rowData(obj)),
      sample_meta=as.data.frame(colData(obj)),
      name=as.character(metadata(obj)$name),
      description=as.character(metadata(obj)$description),
      type=as.character(metadata(obj)$type),
      libraries=as.character(metadata(obj)$libraries)
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
#' export_xlsx(D,'iris_DatasetExperiment.xlsx')
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

#' @export
#' @rdname autocompletion
#' @method .DollarNames DatasetExperiment
.DollarNames.DatasetExperiment <- function(x, pattern = "") {
    IN = c('data', 'sample_meta','variable_meta','name','description','type',
        'libraries','citations','ontology')
    return(IN)
}

#' @export 
#' @rdname autocompletion
setMethod('.DollarNames','DatasetExperiment',.DollarNames.DatasetExperiment)



