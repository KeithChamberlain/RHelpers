##
# Outliers
# Keith Chamberlain
# 2026-11-07
##


outlier.iqr <- function(data, fun=mean, multiplier=1.5, na.rm=FALSE){
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

