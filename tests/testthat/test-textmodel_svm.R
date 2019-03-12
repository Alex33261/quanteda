context("test textmodel_nb")

test_that("the svm model works", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"), 
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- dfm(corp, tolower = FALSE)
    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"))
    
    expect_output(
        print(tmod),
        "Call:"
    )
    
    expect_equal(
        head(coef(tmod), 3),
        c(intercept = 0.16666667, Chinese = 0.09649123, Beijing = 0.09649123),
        tol = .0000001
    )
    
    tmod2 <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), intercept = FALSE)
    
    expect_equal(names(summary(tmod)), c("call", "estimated.feature.scores"))
        
    expect_identical(
        predict(tmod),
        c(d1 = "Y", d2 = "Y", d3 = "N", d4 = "N", d5 = "N")
    )
    
    expect_error(
        textmodel_svm(dfmat, y = c("Y", "N", "Maybe", NA, NA)),
        "y must contain two values only"
    )
})


