
#' Fisher's Iris data
#'
#' Fisher's Iris data as a DatasetExperiment object
#' @export iris_DatasetExperiment
#' @return DatasetExperiment object
#' @import datasets
#' @examples
#' D = iris_DatasetExperiment()
iris_DatasetExperiment = function () {
        iris = datasets::iris
        DatasetExperiment(
            name="Fisher's Iris dataset",
            description=paste0(
            "This famous (Fisher's or Anderson's) iris data set gives ",
            "the measurements in centimeters of the variables sepal length and ",
            "width and petal length and width, respectively, for 50 flowers from ",
            "each of 3 species of iris. The species are Iris setosa, versicolor, ",
            "and virginica."),
            assay = iris[, 1:4],
            rowData = iris[, -(1:4), drop = FALSE],
            colData=data.frame('feature_id'=colnames(iris[,1:4]))
        )
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
#' @param ... named slots and their values.
example_model = function(...) {
    # new object
    out = .example_model()
    # initialise
    out = .initialize_struct_class(out,...)
    return(out)
}




.example_model = setClass('example_model',
    contains = c('model','stato'),
    slots = c(
        'params_value_0' = 'entity',
        'params_value_1' = 'entity_stato',
        'params_value_2' = 'numeric',
        'outputs_result_1' = 'entity',
        'outputs_result_2' = 'DatasetExperiment'
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
        outputs_result_1 = entity(name = 'Result 1',type = 'DatasetExperiment',
            description = 'An example entity object',value = DatasetExperiment()),
        outputs_result_2 = DatasetExperiment(),
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
#' D = iris_DatasetExperiment()
#' M = example_model(value_1 = 10, value_2 = 20)
#' M = model_train(M,D)
setMethod(f = 'model_train',
    signature = c('example_model','DatasetExperiment'),
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
#' D = iris_DatasetExperiment()
#' M = example_model(value_1 = 10, value_2 = 20)
#' M = model_predict(M,D)
setMethod(f = 'model_predict',
    signature = c('example_model','DatasetExperiment'),
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
#' @param ... named slots and their values.
example_chart = function(...) {
    # new object
    out = .example_chart()
    # initialise
    out = .initialize_struct_class(out,...)
    return(out)
}


.example_chart<-setClass(
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











