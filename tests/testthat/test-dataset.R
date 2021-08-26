# test dataset object
test_that('DatasetExperiment objects',{
    # a test dataset object
    # NB will fail if slots cant be assigned
    test_data=iris_DatasetExperiment()

    expect_identical(colnames(test_data$data),colnames(iris[,1:4]))
    expect_identical(rownames(test_data$data),rownames(iris[,1:4]))
    expect_identical(matrix(test_data$data),matrix(iris[,1:4]))
    
    expect_identical(colnames(test_data$sample_meta),colnames(iris[,5,drop=FALSE]))
    expect_identical(rownames(test_data$sample_meta),rownames(iris[,5,drop=FALSE]))
    expect_identical(matrix(test_data$sample_meta),matrix(iris[,5,drop=FALSE]))

    # check access to slots is restricted
    expect_error(test_data$cabbage)
    expect_error({test_data$cabbage='flipflop'})

    # check show
    expect_output(show(test_data),'A "DatasetExperiment" object')
    
    SE=as.SummarizedExperiment(iris_DatasetExperiment())
    expect_true(is(SE,'SummarizedExperiment'))
    
    DE=as.DatasetExperiment(SE)
    expect_true(is(DE,'DatasetExperiment'))

    SE=SummarizedExperiment(data.frame(x=1:8, y=letters[1:8]))
    DE=as.DatasetExperiment(SE)
    expect_true(is(DE,'DatasetExperiment'))
    expect_identical(dimnames(DE), rev(dimnames(SE)))
    
    expect_true("name" %in% .DollarNames.struct_class(DE))
    
    df=as.data.frame(matrix(rnorm(20*3),nrow=20))
    sm=as.data.frame(matrix(rnorm(20*3),nrow=20))
    vm=as.data.frame(matrix(rnorm(3*3),nrow=3))
    DE=DatasetExperiment(data=df,sample_meta=sm,variable_meta=vm)
    
    # excel files
    dirT <- tempdir()
    export_xlsx(DE,file.path(dirT,'test.xlsx'),transpose=FALSE)
    export_xlsx(DE,file.path(dirT,'test2.xlsx'),transpose=TRUE)
    
    # check the output file has the expect sheets
    sn=openxlsx::getSheetNames(file.path(dirT,'test.xlsx'))
    expect_equal(sn,c('data','sample_meta','variable_meta'))
    
    # check dims aftr reading back in
    DF=openxlsx::read.xlsx(file.path(dirT,'test.xlsx'),rowNames=TRUE,colNames=TRUE,sheet = 'data')
    DF2=openxlsx::read.xlsx(file.path(dirT,'test2.xlsx'),rowNames=TRUE,colNames=TRUE,sheet = 'data')
    SM=openxlsx::read.xlsx(file.path(dirT,'test.xlsx'),rowNames=TRUE,colNames=TRUE,sheet='sample_meta')
    VM=openxlsx::read.xlsx(file.path(dirT,'test.xlsx'),rowNames=TRUE,colNames=TRUE,sheet='variable_meta')
    
    expect_equal(dim(DF),dim(df))
    expect_equal(dim(DF2),dim(t(df)))
    expect_equal(dim(SM),dim(sm))
    expect_equal(dim(VM),dim(vm))
    
    # clean up temp files
    files <- dir(path=dirT, pattern="test*")
    unlink(x=files)
})
