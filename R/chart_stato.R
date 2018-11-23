#' chart_stato_class
#'
#' A base class in the \pkg{struct} package. Should not be called directly.
#'
#' Extends the \code{chart} class to include stato functionality.
#' @seealso Refer to \code{\link{chart}} and \code{\link{stato}} for further
#' info.
#'
#' @export chart.stato
#' @include generics.R struct_class.R chart_class.R stato_class.R
#' @examples
#' obj = chart.stato() # create chart object

chart.stato<-setClass(
    "chart.stato",
    contains=c('chart','stato')
)


