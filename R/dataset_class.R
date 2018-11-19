#' dataset class
#'
#' an object for holding raw data and associated meta data
#' @export dataset
#' @slot name (character) name of the dataset
#' @slot description (character) brief description of the dataset
#' @slot data (data.frame) matrix of data, samples in rows, variables/features in columns.
#' @slot sample.meta (data.frame) data frame of sample meta data e.g. group membership
#' @slot variable.meta (data.frame) data frame of variable meta data
#' @param obj a dataset object
#' @param object a dataset object
#' @param x a dataset object
#' @param name name of the slot to set (data, sample_meta or variable_meta for dataset objects)
#' @param value a data.frame
#' @rdname dataset
#' @include generics.R struct_class.R stato_class.R chart_class.R chart_stato.R
#'
dataset<-setClass(
  "dataset",
  contains=c("struct_class"),
  slots=c(name="character",
          description="character",
          data="data.frame",
          sample_meta="data.frame",
          variable_meta="data.frame"
  ),
  prototype=list(name="Dataset000",
                 description="an empty dataset object"
  )

)

#' get the data matrix from a dataset object
#'
#' @export
#' @rdname dataset
setMethod(f="dataset.data",
          signature=c("dataset"),
          definition=function(obj)
          {
            return(obj@data)
          }
)

#' @rdname dataset
#' @export
setMethod(f="$",
          signature=c("dataset"),
          definition=function(x,name)
          {
            s=c('data','sample_meta','variable_meta')
            if (name %in% s)
            {
              value=slot(x,name)
              return(value)
            } else {
              stop(paste0('"',name,'" is not a valid slot for dataset objects'))
            }
          }
)

#' @export
#' @rdname dataset
setMethod(f="dataset.data<-",
          signature=c("dataset"),
          definition=function(obj,value)
          {
            obj@data=value
            return(obj)
          }
)

#' @rdname dataset
#' @export
setMethod(f="$<-",
          signature(x='dataset'),
          definition=function(x,name,value) {
            s=c('data','sample_meta','variable_meta')
            if (name %in% s)
            {
              slot(x,name)=value
              return(x)
            } else {
              stop(paste0('"',name,'" is not a valid slot for dataset objects'))
            }
          }
)

#' get the sample_meta for a dataset object
#'
#' Returns the sample meta data of a dataset object
#' @export
#' @rdname dataset
setMethod(f="dataset.sample_meta",
          signature=c("dataset"),
          definition=function(obj)
          {
            return(obj@sample_meta)
          }
)

#' set the sample_meta for a dataset object
#'
#' Sets the sample meta data of a dataset object
#' @export
#' @rdname dataset
setMethod(f="dataset.sample_meta<-",
          signature=c("dataset"),
          definition=function(obj,value)
          {
            obj@sample_meta=value
            return(obj)
          }
)

#' Get the variable_meta for a dataset object
#'
#' Returns the variable metadata of a dataset object
#' @export
#' @rdname dataset
setMethod(f="dataset.variable_meta",
          signature=c("dataset"),
          definition=function(obj)
          {
            return(obj@variable_meta)
          }
)

#' Set the variable_meta for a dataset object
#'
#' Sets the variable metadata of a dataset object
#' @export
#' @rdname dataset
setMethod(f="dataset.variable_meta<-",
          signature=c("dataset"),
          definition=function(obj,value) {
            obj@variable_meta=value
            return(obj)
          }
)

#' dataset summary
#'
#' print a brief summary of the contents of a dataset object. Note that this is different to the output from show(dataset), which summarieses the dataset object, not the contents..
#' @export
#' @import crayon
#' @rdname dataset
setMethod(f="summary",
          signature=c("dataset"),
          definition=function(object) {
            S=list()
            S$name=name(object)
            S$description=description(object)
            S$type=type(object)
            S$n.samples=nrow(dataset.data(object))
            S$n.features=ncol(dataset.data(object))
            S$n.levels=length(levels(dataset.sample_meta(object)[,1]))
            cat(bold('A',class(object),'object from the struct package') %+%  '\n\n' %+% blue('Name: '),name(object),'\n' %+% blue('Description: '),description(object),'\n',sep='')
            cat('\nConsists of ',S$n.samples,' samples and ',S$n.features,' features.\n',sep='')
            cat('\nThere are ',S$n.levels, ' levels: ',sep='')
            cat(green(levels(dataset.sample_meta(object)[,1])),sep=',')
            cat(' in factor named "',green(names(dataset.sample_meta(object))[1]),'"',sep='')
          }
)
