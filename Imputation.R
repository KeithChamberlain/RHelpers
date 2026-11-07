##
# Imputation Methods
# Keith Chamberlain
##


impute.distribution <- function(y, distribution=rnorm, ...){
    '
    uses distribution to impute missing values
    '
    distribution_of_data <- distribution(n=length(y), ...)
    y[is.na(y)] <- distribution_of_data[is.na(y)]
    y
}


impute.lm.smooth <- function(y, x, degree=2){
    '
    uses fun to impute missing values
    '
    df <- data.frame(y=y, x=as.Date(x))
    nas <- is.na(df$y)
    fit <- lm(y~poly(x, degree=degree), data=df, na.action=na.omit)
    df[nas, "y"] <- predict(fit, newdata=df[nas, ])
    df$y
}


impute.categorical <- function(y){
    '
    imputes categorical missingness
    '
    yy <- sort(table(y), decreasing=TRUE)
    yy <- yy[names(yy) != ""]
    probs <- yy / length(yy)
    y[is.na(y)] <- sample(x=names(yy), size=sum(is.na(y)), prob=probs, replace=TRUE)
    y
}
