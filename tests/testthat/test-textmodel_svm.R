## TO DO:
## - test multi-class values

context("test textmodel_svm")

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
        coef(tmod)[1, 1:3, drop = FALSE],
        matrix(c(-0.3296419, -0.1368896, -0.1368896), nrow = 1, 
               dimnames = list(NULL, c("Chinese", "Beijing", "Shanghai"))),
        tol = .0000001
    )
    
    expect_equal(names(summary(tmod)), c("call", "estimated.feature.scores"))
    expect_identical(
        predict(tmod, type = "class"),
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "Y")
    )
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.4, .4, .4, .6, .5, .6, .6, .6, .4, .5), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("N", "Y"))),
        tol = .1
    )
})

test_that("the svm model works with different weights", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"), 
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- dfm(corp, tolower = FALSE)
    
    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), weight = "docfreq")
    expect_identical(
        predict(tmod, type = "class"),
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "Y", d5 = "Y")
    )
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.3, .3, .4, .5, .3, .7, .7, .6, .5, .7), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("N", "Y"))),
        tol = .1
    )

    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), weight = "termfreq")
    expect_identical(
        predict(tmod, type = "class"),
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "Y", d5 = "Y")
    )
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.3, .3, .4, .5, .3, .7, .7, .6, .5, .7), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("N", "Y"))),
        tol = .1
    )
})

test_that("the svm model works with bias = 0", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"), 
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- dfm(corp, tolower = FALSE)
    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), bias = 0)
    expect_identical(
        predict(tmod, type = "class"),
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "Y")
    )
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.4, .4, .4, .6, .5, .6, .6, .6, .4, .5), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("N", "Y"))),
        tol = .1
    )
})

context("test textmodel_svmlin")

test_that("the svmlin model works", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"), 
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- dfm(corp, tolower = FALSE)
    tmod <- textmodel_svmlin(dfmat, y = docvars(dfmat, "train"), pos_frac = 0.75)
    
    expect_output(
        print(tmod),
        "Call:"
    )
    
    expect_equal(
        head(coef(tmod), 3),
        c(intercept = 0.16666667, Chinese = 0.09649123, Beijing = 0.09649123),
        tol = .0000001
    )
    
    tmod2 <- textmodel_svmlin(dfmat, y = docvars(dfmat, "train"), intercept = FALSE, pos_frac = 0.75)
    expect_identical(
        predict(tmod2),
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "N")
    )
    expect_equal(names(summary(tmod)), c("call", "estimated.feature.scores"))
        
    expect_identical(
        predict(tmod),
        c(d1 = "Y", d2 = "Y", d3 = "N", d4 = "N", d5 = "N")
    )
    
    expect_error(
        textmodel_svmlin(dfmat, y = c("Y", "N", "Maybe", NA, NA)),
        "y must contain two values only"
    )
})

