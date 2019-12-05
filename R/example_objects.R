
#' fishers iris data
#'
#' fishers iris data as a struct dataset object
#' @export iris_dataset
#' @return dataset object
#' @import datasets
#' @examples
#' D = iris_dataset()
#' summary(D)
iris_dataset = function() {
    iris = datasets::iris
    v = data.frame('feature_id' = colnames(iris[,1:4]))
    rownames(v) = colnames(iris[,1:4])
    test_data = dataset(
        name = 'Iris',
        description = "Fisher's Iris data",
        type = 'single_block',
        data = iris[,seq_len(4)],
        sample_meta = iris[,5,drop = FALSE],
        variable_meta = v
    )
    return(test_data)
}

#' Example model
#'
#' An example model for testing
#' @export example_model
#' @return dataset object
#' @param M example_model object
#' @param D dataset object
#' @rdname example_model
#' @include model_class.R
#' @examples
#' M = example_model()
#' M = example_model(value_1 = 10, value_2 = 20)
#'
example_model = setClass('example_model',
    contains = c('model','stato'),
    slots = c(
        'params_value_0' = 'entity',
        'params_value_1' = 'entity_stato',
        'params_value_2' = 'numeric',
        'outputs_result_1' = 'entity',
        'outputs_result_2' = 'dataset'
    ),
    prototype = list(
        name = 'A test model',
        description = 'An example model object. Training adds value_1 counts to
        a dataset, while prediction adds value_2 counts.',
        type = 'test',
        stato_id = 'OBI:0000011',
        params_value_0 = entity(name = 'Value 0',value = 0,type = 'numeric'),
        params_value_1 = entity_stato(value = 10,name = 'Value 1',type = 'numeric',
            description = 'An example entity_stato object',
            stato_id = 'STATO:0000047'),
        params_value_2 = 20,
        outputs_result_1 = entity(name = 'Result 1',type = 'dataset',
            description = 'An example entity object',value = dataset()),
        outputs_result_2 = dataset(),
        predicted = 'result_1'
    )
)

#' model_train example
#'
#' trains the example model, which adds value_1 to the raw data of a dataset
#' @export
#' @return dataset object
#' @import datasets
#' @rdname example_model
#' @examples
#' D = iris_dataset()
#' M = example_model(value_1 = 10, value_2 = 20)
#' M = model_train(M,D)
setMethod(f = 'model_train',
    signature = c('example_model','dataset'),
    definition = function(M,D) {
        D$data = D$data + M$value_1
        M$result_1 = D
        return(M)
    }
)

#' model_predict example
#'
#' predicts using the example model, which adds value_2 to the raw data of a
#' dataset
#' @export
#' @return dataset object
#' @rdname example_model
#' @examples
#' D = iris_dataset()
#' M = example_model(value_1 = 10, value_2 = 20)
#' M = model_predict(M,D)
setMethod(f = 'model_predict',
    signature = c('example_model','dataset'),
    definition = function(M,D) {
        D$data = D$data + M$value_2
        M$result_2 = D
        return(M)
    }
)


#' example chart object
#'
#' an example of a chart object for documentation purposes
#' @export example_chart
#' @rdname chart_example
#' @return a chart object
#' @examples
#' C = example_chart()
#' chart_plot(C,example_model())
#' @importFrom graphics plot
#' @importFrom stats runif
example_chart<-setClass(
    "example_chart",
    contains = c('chart')
)

#' @param obj a chart object
#' @param dobj a example_model object
#' @rdname chart_example
#' @export
setMethod(f = "chart_plot",
    signature = c("example_chart","example_model"),
    definition = function(obj,dobj)
    {
        p = plot(runif(n = 10),runif(n = 10))
        return(p)
    }
)











