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
          charts.boxplot="chart.stato",
          charts.mv_histogram="chart",
          charts.mv_boxplot="chart"
  ),
  prototype=list(name="Dataset000",
                 description="an empty dataset object",
                 charts=c('boxplot','mv_histogram','mv_boxplot'),
                 charts.boxplot=chart.stato(name='box and whisker plot',
                                            description='Boxplots showing the range of observed values for feature, separated by group if a factor.',
                                            type='boxplot',
                                            fcn=dataset_boxplot_fcn,
                                            opt=list(label_outliers=TRUE, # label outliers
                                                     feature_to_plot='V1',   #  feature to plot by label
                                                     factor_name='factor',   # name of factor to appear on legend
                                                     show_counts=TRUE
                                            ),
                                            stato.id='STATO:0000243'),

                 charts.mv_histogram=chart(name='Missing values histogram',
                                           description='a histogram of the % missing values per sample/feature',
                                           type='histogram',
                                           fcn=missing_value_histogram,
                                           opt=list(by_sample=TRUE)),

                 charts.mv_boxplot=chart(name='Missing values boxplot',
                                           description='a histogram of the % missing values per sample/feature',
                                           type='histogram',
                                           fcn=missing_value_boxplot,
                                           opt=list(by_sample=TRUE,
                                                    factor_name='factor')
                 )
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

#' @import ggplot2 ggthemes ggrepel
#' @importFrom pmp createClassAndColors theme_Publication scale_colour_Publication
#' @importFrom sp point.in.polygon
#' @importFrom scales squish
#' @export
setMethod(f="chart.plot",
          signature="dataset",
          definition = function(obj,name,opt=NULL) {
            # check if valid chart name
            is.chart(obj,name) # error if not

            # if opt is null get defaults for this chart
            if (is.null(opt))
            {
              opt=chart.opt(obj,name)
            }

            cobj=chart.obj(obj,name)

            return(cobj@fcn(obj,opt))
          }

)

#' @export
setMethod(f="chart.opt",
          signature='dataset',
          definition=function(obj,name)
          {
            x=chart.obj(obj,name)
            return(x@opt)
          }
)
