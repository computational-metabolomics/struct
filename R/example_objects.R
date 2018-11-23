
#' fishers iris data
#'
#' fishers iris data as a struct dataset object
#' @export iris_dataset
#' @return dataset object
#' @import datasets
#' @examples
#' D = iris_dataset()
#' summary(D)
iris_dataset=function() {
    iris=datasets::iris
    v=data.frame('sample_id'=rownames(iris))
    rownames(v)=rownames(iris)
    test_data = dataset(
        name = 'Iris',
        description = "Fisher's Iris data",
        type='single_block',
        data=iris[,seq_len(4)],
        sample_meta=iris[,5,drop=FALSE],
        variable_meta=v
    )
    return(test_data)
}

#' Example model
#'
#' An example model for testing
#' @export test_model
#' @return dataset object
#' @import datasets
#' @param M test_model object
#' @param D dataset object
#' @rdname test_model
#' @include model_class.R model_stato_class.R
#' @examples
#' M = test_model()
#' M = test_model(value_1 = 10, value_2 = 20)
test_model=setClass('test_model',
    contains = 'model.stato',
    slots=c(
        'params.value_1'='entity.stato',
        'params.value_2'='numeric',
        'outputs.result_1'='entity',
        'outputs.result_2'='numeric'
    ),
    prototype = list(
        name='A test model',
        description='An example model object. Training adds value_1 counts to
        a dataset, while prediction adds value_2 counts.',
        type='test',
        stato.id='OBI:0000011',
        params.value_1=entity.stato(value=10,name='Value 1',type='numeric',
            description='An example entity object',
            stato.id='STATO:0000047'),
        params.value_2=20,
        outputs.result_1=entity(name='Result 1',type='dataset',
            description='An example entity object'),
        outputs.result_2=2
    )
)

#' model.train example
#'
#' trains the example model, which adds value_1 to the raw data of a dataset
#' @export
#' @return dataset object
#' @import datasets
#' @rdname test_model
#' @examples
#' D = iris_dataset()
#' M = test_model(value_1 = 10, value_2 = 20)
#' M = model.train(M,D)
setMethod(f='model.train',
    signature=c('test_model','dataset'),
    definition = function(M,D) {
        D=D$data+M$value_1
        M$result_1=D
    }
)

#' model.predict example
#'
#' predicts using the example model, which adds value_2 to the raw data of a
#' dataset
#' @export
#' @return dataset object
#' @import datasets
#' @rdname test_model
#' @examples
#' D = iris_dataset()
#' M = test_model(value_1 = 10, value_2 = 20)
#' M = model.predict(M,D)
setMethod(f='model.predict',
    signature=c('test_model','dataset'),
    definition = function(M,D) {
        D=D$data+M$value_2
        M$result_2=D
    }
)



