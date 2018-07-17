#' chartable_class
#'
#' A base class in the \pkg{struct} package. Provides several fundamental methods for charting and should not be called directly.
#' @export chartable_class
#' @include generics.R struct_class.R
#'
chartable_class<-setClass(
  "chartable_class",
  slots=c('charts'="character"
  )
)


#' @export
setMethod(f="chart.obj",
          signature=c("chartable_class","character"),
          definition=function(obj,name)
          {
            value=slot(obj, paste("charts",name,sep='.'))
            return(value)
          }
)

#' @export
setMethod(f="chart.obj<-",
          signature=c("chartable_class","character"),
          definition=function(obj,name,value)
          {
            slot(obj, paste("charts",name,sep='.'))=value
            return(obj)
          }
)


setMethod(f="chart.plot",
          signature="chartable_class",
          definition=function(obj,name,opt=NULL)
          {
            c=chart(obj,name)
            if (is(c,'function'))
            {
              out=c(obj,opt)
            }
            if (is(c,'chart') | is(c,'chart.stato'))
            {
              out=c@fcn(obj,opt)
            }
            return(out)
          }
)

setMethod(f="chart.ids",
          signature="chartable_class",
          definition=function(obj)
          {
            return(obj@charts)
          }
)

#' @export
setMethod(f="chart.name",
          signature=c("chartable_class",'character'),
          definition=function(obj,name)
          {
            p=slot(obj, paste("charts",name,sep='.'))
            # if a chart object then get its name
            if (is(p,'chart'))
            {
              value=name(p)
            }
            else
            {
              # otherwise just return the slot name
              value=slot(obj, paste("charts",name,sep='.'))
            }
            return(value)
          }
)

setMethod(f="chart.opt",
          signature="chartable_class",
          definition=function(obj,name)
          {
            return(obj@charts.opt[[name]])
          }
)

setMethod(f="chart.opt<-",
          signature=c("chartable_class",'character'),
          definition=function(obj,name,value)
          {
            obj@charts.opt[[name]]<-value
            return(obj)
          }
)

setMethod(f="is.chart",
          signature=c("chartable_class"),
          definition=function(obj,name)
          {
            valid=obj@charts
            if (name %in% valid)
            {
              return(TRUE)
            }
            else
            {
              stop('"', name, '" is not valid chart for an object of class "', class(obj)[1],'"')
            }
          }
)

