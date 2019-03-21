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
    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), scale = TRUE)
    
    expect_output(
        print(tmod),
        "Call:"
    )
    
    expect_equal(
        coef(tmod)[1, 1:3, drop = FALSE],
        matrix(c(0.2706701, 0.1562714, 0.1562714), nrow = 1, 
               dimnames = list(NULL, c("Chinese", "Beijing", "Shanghai"))),
        tol = .0000001
    )
    
    expect_equal(names(summary(tmod)), c("call", "estimated.feature.scores"))
    expect_identical(
        predict(tmod, type = "class"),
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "N")
    )
    set.seed(10)
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.8, .8, .8, .2, .4, .2, .2, .2, .8, .6), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("Y", "N"))),
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
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "N")
    )
    set.seed(10)
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.8, .8, .8, .3, .4, .2, .2, .2, .7, .6), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("Y", "N"))),
        tol = .1
    )

    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), weight = "termfreq")
    expect_identical(
        predict(tmod, type = "class"),
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "N")
    )
    set.seed(10)
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.8, .8, .8, .3, .4, .2, .2, .2, .7, .6), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("Y", "N"))),
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
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "N")
    )
    set.seed(10)
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.7, .7, .7, .1, .2, .3, .3, .3, .9, .8), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("Y", "N"))),
        tol = .1
    )
})

test_that("multiclass prediction works", {
    dfmat <- dfm(data_corpus_irishbudget2010)
    tmod2 <- textmodel_svm(dfmat, y = c(rep(NA, 3), "SF", "FF", "FG", NA, "LAB", NA, NA, "Green", rep(NA, 3)))
    expect_equal(
        head(predict(tmod2), 3),
        c("Lenihan, Brian (FF)" = "FF", 
          "Bruton, Richard (FG)" = "Green",
          "Burton, Joan (LAB)" = "SF")
    )
})

test_that("scale = FALSE works differently", {
    corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"), 
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- dfm(corp, tolower = FALSE)
    tmodscaled <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), scale = TRUE)
    tmodunscaled <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), scale = FALSE)
    
    expect_output(print(tmodscaled), "7 fitted features \\(scaled\\)\\.")
    expect_output(print(tmodunscaled), "7 fitted features\\.")
    
    expect_equal(
        coef(tmodunscaled)[1, "Chinese"],
        c(Chinese = 0.5535941),
        tol = .0000001
    )
    expect_equal(
        coef(tmodscaled)[1, "Chinese"],
        c(Chinese = 0.2706701),
        tol = .0000001
    )
    
    set.seed(10)
    expect_equal(
        predict(tmodunscaled, type = "probability")["d4", "Y"],
        0.4559291
    )
    set.seed(10)
    expect_equal(
        predict(tmodscaled, type = "probability")["d4", "Y"],
        0.2247975
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

