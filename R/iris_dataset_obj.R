
#' fishers iris data
#'
#' fishers iris data as a struct dataset object
#' @export iris_dataset
#' @return dataset object
#' @examples
#' D = iris_dataset()
#' summary(D)
iris_dataset=function() {
    v=data.frame('sample_id'=rownames(iris))
    rownames(v)=rownames(iris)
    test_data = dataset(
        name = 'Iris',
        description = "Fisher's Iris data",
        type='single_block',
        data=iris[,1:4],
        sample_meta=iris[,5,drop=FALSE],
        variable_meta=v
    )
    return(test_data)
}




#' internal use only
#'
#' internal use only
#' @include model_class.R
test_model=setClass('test_model',
    contains='model',
    slots=c(
        params.value_1='numeric',
        params.value_2='numeric',
        outputs.result_1='dataset',
        outputs.result_2='dataset'
    ),
    prototype=list(predicted='result_2')
)


