#' Linear SVM classifier for texts
#' 
#' Fit a fast linear SVM classifier for texts, using the
#' \pkg{LiblineaR} package.
#' @param x the \link{dfm} on which the model will be fit.  Does not need to
#'   contain only the training documents.
#' @param y vector of training labels associated with each document identified 
#'   in \code{train}.  (These will be converted to factors if not already 
#'   factors.)
#' @param weight weights for different classes for imbalanced training sets,
#'   passed to \code{wi} in \code{\link[LiblineaR]{LiblineaR}}. \code{"uniform"}
#'   uses default; \code{"docfreq"} weights by the number of training examples,
#'   and \code{"termfreq"} by the relative sizes of the training classes in
#'   terms of their total lengths in tokens.
#' @param ... additional arguments passed to \code{\link[LiblineaR]{LiblineaR}}
#' @references 
#' R. E. Fan, K. W. Chang, C. J. Hsieh, X. R. Wang, and C. J. Lin. (2008)
#' LIBLINEAR: A Library for Large Linear Classification.
#' \emph{Journal of Machine Learning Research} 9: 1871-1874.
#' \url{http://www.csie.ntu.edu.tw/~cjlin/liblinear}.
#' @seealso \code{\link[LiblineaR]{LiblineaR}}
#' @examples
#' # use Lenihan for govt class and Bruton for opposition
#' docvars(data_corpus_irishbudget2010, "govtopp") <- c("Govt", "Opp", rep(NA, 12))
#' dfmat <- dfm(data_corpus_irishbudget2010)
#' 
#' tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "govtopp"))
#' predict(tmod)
#' predict(tmod, type = "probability")
#' @export
textmodel_svm <- function(x, y, weight = c("uniform", "docfreq", "termfreq"), ...) {
    UseMethod("textmodel_svm")
}

#' @export
textmodel_svm.default <- function(x, y, weight = c("uniform", "docfreq", "termfreq"), ...) {
    stop(friendly_class_undefined_message(class(x), "textmodel_svm"))
}    
    
#' @export
#' @importFrom LiblineaR LiblineaR
#' @importFrom SparseM as.matrix.csr
textmodel_svm.dfm <- function(x, y, weight = c("uniform", "docfreq", "termfreq"), ...) {
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    call <- match.call()
    weight <- match.arg(weight)
    
    y <- factor(y)
    
    temp <- x[!is.na(y), ]
    class <- y[!is.na(y)]
    temp <- dfm_group(temp, class)
    
    if (weight == "uniform") {
        wi <- NULL
    } else if (weight == "docfreq") {
        wi <- rowSums(as.matrix(table(class)))
        wi <- wi / sum(wi)
    } else if (weight == "termfreq") {
        wi <- rowSums(temp)
        wi <- wi / sum(wi)
    }
    
    svmlinfitted <- LiblineaR::LiblineaR(data = as.matrix.csr.dfm(temp),
                                         target = factor(docnames(temp), levels = docnames(temp)),
                                         wi = wi,
                                         ...)
    colnames(svmlinfitted$W)[seq_along(featnames(x))] <- featnames(x)
    result <- list(
        x = x, y = y,
        weights = svmlinfitted$W,
        algorithm = svmlinfitted$TypeDetail,
        type = svmlinfitted$Type,
        classnames = svmlinfitted$ClassNames,
        bias = svmlinfitted$Bias,
        svmlinfitted = svmlinfitted,
        call = call
    )
    names(result$weights) <- featnames(x)
    class(result) <- c("textmodel_svm", "textmodel", "list")
    result
}

# helper methods ----------------

#' Prediction from a fitted textmodel_svm object
#' 
#' \code{predict.textmodel_svm()} implements class predictions from a fitted
#' linear SVM model.
#' @param object a fitted linear SVM textmodel 
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force make newdata's feature set conformant to the model terms
#' @param ... not used
#' @return \code{predict.textmodel_svm} returns either a vector of class
#'   predictions for each row of \code{newdata} (when \code{type = "class"}), or
#'   a document-by-class matrix of class probabilities (when \code{type =
#'   "probability"}).
#' @seealso \code{\link{textmodel_svm}}
#' @keywords textmodel internal
#' @importFrom SparseM as.matrix.csr
#' @export
predict.textmodel_svm <- function(object, newdata = NULL, 
                                  type = c("class", "probability"),
                                  force = FALSE, ...) {
    unused_dots(...)
    
    type <- match.arg(type)
    
    if (!is.null(newdata)) {
        data <- as.dfm(newdata)
    } else {
        data <- as.dfm(object$x)
    }

    # the seq_along is because this will have an added term "bias" at end if bias > 0
    data <- force_conformance(data, names(object$weights)[seq_along(featnames(object$x))], force)
    
    pred_y <- predict(object$svmlinfitted, 
                      newx = as.matrix.csr.dfm(data),
                      proba = (type == "probability"))
    
    if (type == "class") {
        pred_y <- as.character(pred_y$predictions)
        names(pred_y) <- docnames(data)
    } else if (type == "probability") {
        pred_y <- pred_y$probabilities
        rownames(pred_y) <- docnames(data)
    }
    
    pred_y
}

#' @export
#' @method print textmodel_svm
print.textmodel_svm <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        length(na.omit(x$y)), " training documents; ",
        nfeat(na.omit(x)), " fitted features.",
        "\n",
        "Method: ", x$algorithm, "\n",
        sep = "")
}

#' summary method for textmodel_svm objects
#' @param object output from \code{\link{textmodel_svm}}
#' @param n how many coefficients to print before truncating
#' @param ... additional arguments not used
#' @keywords textmodel internal
#' @method summary textmodel_svm
#' @export
summary.textmodel_svm <- function(object, n = 30, ...) {
    result <- list(
        "call" = object$call,
        "estimated.feature.scores" = as.coefficients_textmodel(head(coef(object), n))
    )
    as.summary.textmodel(result)
}

#' @noRd
#' @method coef textmodel_svm
#' @export
coef.textmodel_svm <- function(object, ...) {
    object$weights
}

#' @noRd
#' @method coefficients textmodel_svm
#' @export
coefficients.textmodel_svm <- function(object, ...) {
    UseMethod("coef")   
}

#' @export
#' @method print predict.textmodel_svm
print.predict.textmodel_svm <- function(x, ...) {
    print(unclass(x))
}

#' convert a dfm into a matrix.csr from SparseM package
#' 
#' Utility to convert a dfm into a \link[SparseM]{matrix.csr} from the \pkg{SparseM} package.
#' @param x input \link{dfm}
#' @importFrom SparseM as.matrix.csr
#' @method as.matrix.csr dfm
#' @keywords internal
as.matrix.csr.dfm <- function(x) {
    # convert first to column sparse format
    as.matrix.csr(new("matrix.csc", 
                      ra = x@x,
                      ja = x@i + 1L,
                      ia = x@p + 1L,
                      dimension = x@Dim))
}
