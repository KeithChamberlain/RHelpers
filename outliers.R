##
# Outliers
# Keith Chamberlain
# 2026-11-07
##

#' @title Identify Outliers
#'
#' @description
#' Functions to resolve records meeting spefified criteria as outliers.
#'
#' @usage
#' `outlier.iqr(data, multiplier=1.5, na.rm=FALSE)`
#' `outlier.mad(data, fun=median, multiplier=3, na.rm=FALSE)`
#' `outlier.quantile(data, quantiles=c(0.02, 0.98), na.rm=FALSE)`
#' `outlier.test(data, test=pf, threshold=0.025, ...)`
#' `outlier.test.formula(data, formula, test=pf, threshold=0.025, ...)`
#'
#' @param data numeric vector.
#' @param multiplier range multiplier for IQR and MAD.
#' @param fun center function.
#' @param quantiles vector of low, high quantile threshold values.
#' @param test probability distribution function.
#' @param threshold cutoff threshold for identifying outliers.
#' @param formula character or `formula` model representation.
#' @param na.rm logical; if TRUE, remove NAs.
#' @param ... Other options to control the test function.
#'
#' @value A logical vector indicating `TRUE` for an outlier and `FALSE`
#'     otherwise.
#'
#' @seealso [stats::mad()] for the median absolute deviation. [stats::IQR()]
#'     for the interquartile range. [score()] for an interface to z-score like
#'     transformations. [Distribution] for standard distributions, including,
#'     e.g. [stats::pnorm()], [stats::pnbinom()], or [stats::pbeta()] for the
#'     normal, negative binomial and beta probability distribution functions.
#'
#' @examples:
#'
#' ## Generate Data
#' data <- rnorm(1000, mean=3, sd=5)
#' x <- 1:length(data)
#'
#' ## Identify Outliers
#' ol <- outlier.test(data=data, test=pnorm, threshold=1/length(data),
#'                    mean=mean(data), sd=sd(data))
#' ## Visualize
#' plot(data~x, col=ifelse(ol, "darkred", "blue"))
#'
#' @export


outlier.iqr <- function(data, multiplier=1.5, na.rm=FALSE){
    '
    outliers by iqr. Returns a logical vector identifying outliers.
    '
    lower <- quantile(data, 1/4, na.rm=na.rm)
    upper <- quantile(data, 3/4, na.rm=na.rm)
    iqr <- upper - lower
    data < lower - iqr*multiplier | data > upper + iqr*multiplier
}


outlier.mad <- function(data, fun=median, multiplier=3, na.rm=FALSE){
    '
    outliers by mad. Returns a logical vector identifying outliers.
    '
    median_abs_dev <- mad(data, na.rm=na.rm)
    fun_dev <- fun(data, na.rm=na.rm)
    lower <- fun_dev - multiplier*median_abs_dev
    upper <- fun_dev + multiplier*median_abs_dev
    data < lower | data > upper
}


outlier.quantile <- function(data, quantiles=c(0.02, 0.98), na.rm=FALSE){
    '
    Outiers by quantiles. Returns a logical vector identifying outliers.
    '
    (data < quantile(data, quantiles[1], na.rm=na.rm) |
         data > quantile(data, quantiles[2], na.rm=na.rm))
}


outlier.test <- function(data, test=pf, threshold=0.025, ...){
    '
    Outliers by test statistic. Returns a logical vector identifying outliers.
    '
    test(data, ...) < threshold | test(data, ...) > 1-threshold
}


outlier.test.formula <- function(data, formula, threshold=1/nrow(data),
                                 model_fun=lm, ...){
    '
    Outliers by multivariate test statistic using a formula
    '
    resid <- resid(model_fun(formula=formula, data=data))
    result <- rnorm(resid, mean=mean(resid), sd=sd(resid), ...)
    result < threshold | result > 1-threshold
}
