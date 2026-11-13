#$
# Transform Functions
# Keith Chamberlain
##


#' @title Data Transformations
#'
#' @description
#' Functions to perform data transformations.
#'
#' @usage
#' `score(y, fun=mean, spread_agg=sum, power=2, var_power=1/2, ladder=I, ...)`
#'
#' @param y numeric vector to transform.
#' @param fun central tendency function.
#' @param spread_agg function used with spread determination.
#' @param power power to apply to deviation definition.
#' @param var_power power to apply to the spread definition.
#' @param ladder initial data transformation to apply.
#' @param ... Other options to control the function fun.
#'
#' @value A vector of the transformed values
#'
#' @references
#' Johnson, Kotz & Balakrishnan, Continuous Univariate Distributions,
#'    Vol. 1, Ch. 14 (Wiley, 1994).
#'
#' @seealso [base::scale()] for scaling and centering matrices.
#'
#' @examples:
#'
#' ## Generate Normal and Zero Bounded Data
#' data <- rnorm(10000, mean=3, sd=5)
#' log_scale <- method_of_moments_log(mean=8, sd=4)
#' data2 <- exp(rnorm(10000, mean=log_scale['mu'], sd=log_scale['sigma']))
#' x <- 1:length(data)
#'
#' ## z-transform of Data
#' z <- score(y=data, fun=mean, spread_agg=sum, power=2, var_power=1/2,
#'            ladder=I, ...)
#'
#' ## Scale using Median Deviation and Variance.
#' m <- score(y=data, fun=median, spread_agg=sum, power=1, var_power=1)
#'
#' ##
#' ## Visualize
#' plot(z~x, type="p")
#' plot(m~x, type="p")
#' hist(data2)
#'
#' @export


score <- function(y, fun=mean, spread_agg=sum, power=2,
                  var_power=1/2, ladder=I, ...){
    '
    calculate a transform of y.
    - z-score: fun=mean, spread_agg=sum, power=2, and var_power is 1/2
    - a median-score: fun=median, spread_agg=sum, power=1, var_power=1
    '
    y<-ladder(y)
    (y-fun(y, ...))/(
        spread_agg(abs(y-fun(y, ...))^power)/(length(y)-1))^var_power
}


method_of_moments_log <- function(mean, sd){
    '
    method of moments transformation between mean and sd to log scale.
    '
    c("sigma"=sqrt(log(1+sd^2/mean^2)), "mu"=log(mean) - s^2/2)
}
