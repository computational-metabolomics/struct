#' dataset class
#'
#' an object for holding raw data and associated meta data
#' @export dataset
#' @slot name (character) name of the dataset
#' @slot description (character) brief description of the dataset
#' @slot data (data.frame) matrix of data, samples in rows, variables/features in columns.
#' @slot sample.meta (data.frame) data frame of sample meta data e.g. group membership
#' @slot variable.meta (data.frame) data frame of variable meta data
#' @rdname dataset
#' @include generics.R struct_class.R chartable_class.R stato_class.R chart_class.R chart_stato.R dataset_chart_fcns.R
#'
dataset<-setClass(
  "dataset",
  contains=c("struct_class",'chartable_class'),
  slots=c(name="character",
          description="character",
          data="data.frame",
          main.factor='character',
          sample.meta="data.frame",
          variable.meta="data.frame",
          charts.boxplot="chart.stato"
  ),
  prototype=list(name="Dataset000",
                 description="an empty dataset object",
                 charts='boxplot',
                 charts.boxplot=chart.stato(name='box and whisker plot',
                                            description='Boxplots showing the range of observed values for feature, separated by group.',
                                            type='boxplot',
                                            fcn=dataset_boxplot_fcn
                                            opt=list(label_outliers=TRUE, # label outliers
                                                     feature_to_plot='V1',   #  feature to plot by label
                                                     factor_name='factor',   # name of factor to appear on legend
                                                     groups=factor(), # factor of groups
                                                     feature_labels=character(), # feature labels
                                                     sample_labels=character() # sample labels,
                                            ),
                                            stato.id='STATO:0000243')

  )

)

#' @export
#' @param obj a dataset object
#' @rdname dataset
setMethod(f="data",
          signature=c("dataset"),
          definition=function(obj)
          {
            return(obj@data)
          }
)

#' @export
#' @rdname dataset
setMethod(f="data<-",
          signature=c("dataset"),
          definition=function(obj,value)
          {
            obj@data=value
            return(obj)
          }
)

#' @export
#' @rdname dataset
setMethod(f="sample.meta",
          signature=c("dataset"),
          definition=function(obj)
          {
            return(obj@sample.meta)
          }
)

#' @export
#' @rdname dataset
setMethod(f="sample.meta<-",
          signature=c("dataset"),
          definition=function(obj,value)
          {
            obj@sample.meta=value
            return(obj)
          }
)

#' @export
#' @rdname dataset
setMethod(f="variable.meta",
          signature=c("dataset"),
          definition=function(obj)
          {
            return(obj@variable.meta)
          }
)

#' @export
#' @rdname dataset
setMethod(f="variable.meta<-",
          signature=c("dataset"),
          definition=function(obj,value)
          {
            obj@variable.meta=value
            return(obj)
          }
)

#' @export
setMethod(f="show",
          signature=c("dataset"),
          definition=function(object)
          {
            cat('A dataset object\nName: ',name(object),'\nDescription: ',description(object),sep='')
          }
)


