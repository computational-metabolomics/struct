
#' Fisher's Iris data
#'
#' Fisher's Iris data as a DatasetExperiment object
#' @export
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
            data = iris[, 1:4],
            sample_meta = iris[, -(1:4), drop = FALSE],
            variable_meta=data.frame('feature_id'=colnames(iris[,1:4])),
            citations='Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.'
        )
    }

#' Example model
#'
#' An example model for testing. Training this model adds value_1 to a data set, and
#' prediction using this model adds value_2.
#' @export
#' @return modified example_model object
#' @param M A struct model object
#' @param D A DatasetExperiment object
#' @param value_0 a numeric value
#' @param value_1 a numeric value
#' @param value_2 a numeric value
#' @param ... additional slots and values to pass to struct_class 
#' @rdname example_model
#' @include model_class.R
#' @examples
#' M = example_model()
#' M = example_model(value_1 = 10, value_2 = 20)
#' @param ... named slots and their values.
example_model = function(value_0=0,value_1=10,value_2=20,...) {
    # new object
    out = new_struct('example_model',value_0=value_0,value_1=value_1,value_2=value_2,...)
    return(out)
}

.example_model = setClass('example_model',
    contains = c('model','stato'),
    slots = c(
        'value_0' = 'entity',
        'value_1' = 'entity_stato',
        'value_2' = 'numeric',
        'result_1' = 'entity',
        'result_2' = 'DatasetExperiment'
    ),
    prototype = list(
        name = 'A test model',
        description = 'An example model object. Training adds value_1 counts to
        a dataset, while prediction adds value_2 counts.',
        type = 'test',
        stato_id = 'OBI:0000011',
        value_0 = entity(name = 'Value 0',value = 0,type = 'numeric'),
        value_1 = entity_stato(value = 10,name = 'Value 1',type = 'numeric',
            description = 'An example entity_stato object',
            stato_id = 'STATO:0000047'),
        value_2 = 20,
        result_1 = entity(name = 'Result 1',type = 'DatasetExperiment',
            description = 'An example entity object',value = DatasetExperiment()),
        result_2 = DatasetExperiment(),
        predicted = 'result_1',
        .params=c('value_0','value_1','value_2'),
        .outputs=c('result_1','result_2')
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
    out = .example_chart(...)
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











