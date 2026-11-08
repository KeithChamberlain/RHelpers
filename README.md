RHelpersREADME.Rmd
================
Keith Chamberlain
2026-11-08

# README

Demonstrating and documenting the helper functions in this repo.

1.  (if not installed) installing and loading a vector of package names.
2.  Univariate imputation procedures for comparison
3.  Univariate outlier imputation

``` r
source("Imputation.R")
source("LoadingPackages.R")
source("outliers.R")

packages <- c("ggplot2", "patchwork", "gamlss.tr")
N <- 10000

# (If not installed) Install and Load Packages
installed <- install_missing_packages(packages)
loaded <- load_packages(packages)
```

    ## Loading required package: gamlss.dist

    ## Loading required package: gamlss

    ## Loading required package: splines

    ## Loading required package: gamlss.data

    ## 
    ## Attaching package: 'gamlss.data'

    ## The following object is masked from 'package:datasets':
    ## 
    ##     sleep

    ## Loading required package: nlme

    ## Loading required package: parallel

    ##  **********   GAMLSS Version 5.5-0  **********

    ## For more on GAMLSS look at https://www.gamlss.com/

    ## Type gamlssNews() to see new features/changes/bug fixes.

``` r
gen.trun(par=c(0), family="NO", type=c("left"), name="0")
```

    ## A truncated family of distributions from NO has been generated 
    ##  and saved under the names:  
    ##  dNO0 pNO0 qNO0 rNO0 NO0 
    ## The type of truncation is left 
    ##  and the truncation parameter is 0

# Create Data

Create data for methods

``` r
data <- rnorm(n=10000, mean=30, sd=1)
data_nas <- sample(x=c(data, rep(NA, 400)), size=N, replace = TRUE)
data_categorical <- sample(x=c("Group A", "Group B", NA), size=N,
                           prob=c(0.6, 0.3, 0.1), replace=TRUE)
times <- 1:N

print("Number of NAs in data")
```

    ## [1] "Number of NAs in data"

``` r
sum(is.na(data_nas))
```

    ## [1] 414

``` r
print("Number of NAs in categorical")
```

    ## [1] "Number of NAs in categorical"

``` r
sum(is.na(data_categorical))
```

    ## [1] 999

# Imputation Fun

Demonstrate imputation procedures that improve upon imputation by mean
or median values.

1.  fill by median value
2.  fill by sampling from a distribution
3.  fill by smoothing using lm
4.  fill by sampling from non NA categories

For impute.distribution, `...` passes the specified distribution
function parameters such as the mean, sd, sigma, etc.

``` r
data_fill_median <- impute.fun(data_nas, fun=median)
data_fill_nas_distribution <- impute.distribution(
    y=data_nas, distribution=rnorm, mean=mean(data_nas, na.rm=TRUE),
    sd=sd(data_nas, na.rm=TRUE))
data_fill_nas_smooth <- impute.lm.smooth(y=data_nas, x=times, degree=2)
data_fill_nas_categorical <- impute.categorical(y=data_categorical)

print("Number of NAs in data after fill by fun")
```

    ## [1] "Number of NAs in data after fill by fun"

``` r
sum(is.na(data_fill_median))
```

    ## [1] 0

``` r
print("Number of NAs in data after fill by distribution")
```

    ## [1] "Number of NAs in data after fill by distribution"

``` r
sum(is.na(data_fill_nas_distribution))
```

    ## [1] 0

``` r
print("Number of NAs in data after fill by smooth")
```

    ## [1] "Number of NAs in data after fill by smooth"

``` r
sum(is.na(data_fill_nas_smooth))
```

    ## [1] 0

``` r
print("Number of NAs in data after fill by category")
```

    ## [1] "Number of NAs in data after fill by category"

``` r
sum(is.na(data_fill_nas_categorical))
```

    ## [1] 0

# Visualization

View distribution before/after imputation

``` r
dat <- data.frame(nas=data_nas, nas_cat=data_categorical,
                  median=data_fill_median, distrib=data_fill_nas_distribution,
                  smooth=data_fill_nas_smooth, cat=data_fill_nas_categorical)

pltmedian <- ggplot(data=dat) +
    geom_histogram(fill="steelblue", alpha=0.4, aes(x=nas), bins=30) +
    geom_histogram(fill="darkred", alpha=0.4, aes(x=median), bins=30) +
    labs(x="Data with NAs and Median Imputation", y="Counts",
         title="Median Imputation Inflates the Median")

pltsmooth <-  ggplot(data=dat) +
    geom_histogram(fill="steelblue", alpha=0.4 ,aes(x=nas), bins=30) +
    geom_histogram(fill="darkred", alpha=0.4, aes(x=smooth), bins=30) +
    labs(x="Data and Smoothing Imputation", y="Counts", 
         title="Smoothing Imputation Inflates Center")

pltdistrib <- ggplot(data=dat) +
    geom_histogram(fill="steelblue", alpha=0.4 ,aes(x=nas), bins=30) +
    geom_histogram(fill="darkred", alpha=0.4, aes(x=distrib), bins=30) +
    labs(x="Data and Distributional Imputation", y="Counts", 
         title="Distribution Imputation Maintains\nthe Distributions Shape")

pltcats <- ggplot(data=dat) +
    geom_bar(aes(x=nas_cat), stat="count", alpha=0.4, fill="steelblue") +
    geom_bar(aes(x=cat), stat="count", alpha=0.4, fill="darkred") +
    labs(x="Group", y="Count",
         title="Imputing by Category Shape\nPreserves Non NA Categories")
    
plt <- (pltmedian + pltdistrib) / (pltsmooth + pltcats)
plt
```

    ## Warning: Removed 414 rows containing non-finite outside the scale range (`stat_bin()`).
    ## Removed 414 rows containing non-finite outside the scale range (`stat_bin()`).
    ## Removed 414 rows containing non-finite outside the scale range (`stat_bin()`).

![](RHelpersREADME_files/figure-gfm/visualizations-1.png)<!-- -->

``` r
ggsave("RHelpersREADMEImputation.jpeg", plt, width=10, height=10)
```

    ## Warning: Removed 414 rows containing non-finite outside the scale range (`stat_bin()`).
    ## Removed 414 rows containing non-finite outside the scale range (`stat_bin()`).
    ## Removed 414 rows containing non-finite outside the scale range (`stat_bin()`).

# Outiers

Handling outliers identifying by iqr, mad and quantiles and filling with
distrubtional imputation

``` r
data_trun <- rNO0(n=N, mu=3, sigma=10)

data_outliers_filled <- data_outliers <- sample(
    x=c(data_trun, (data_trun+3*sd(data_trun))[1:200], rep(NA, 300)), 
    size=N, replace=TRUE)

outliers_by_iqr <- outlier.iqr(data_outliers, na.rm=TRUE)
outliers_by_mad <- outlier.mad(data_outliers, na.rm=TRUE)
outliers_by_quantiles <- outlier.quantile(data_outliers, na.rm=TRUE)

data_outliers_filled[outliers_by_iqr | outliers_by_mad | outliers_by_quantiles] <- NA

data_outliers_filled <- impute.distribution(
    y=data_outliers_filled, distribution=rNO0, 
    mu=mean(data_outliers_filled, na.rm=TRUE),
    sigma=sd(data_outliers_filled, na.rm=TRUE)
)

print("Percent outliers by IQR")
```

    ## [1] "Percent outliers by IQR"

``` r
sum(outliers_by_iqr, na.rm=TRUE)/length(outliers_by_iqr)*100
```

    ## [1] 2.02

``` r
print("Percent outiers by MAD")
```

    ## [1] "Percent outiers by MAD"

``` r
sum(outliers_by_mad, na.rm=TRUE)/length(outliers_by_mad)*100
```

    ## [1] 1.81

``` r
print("Percent outliers by quantiles")
```

    ## [1] "Percent outliers by quantiles"

``` r
sum(outliers_by_quantiles, na.rm=TRUE)/length(outliers_by_quantiles) * 100
```

    ## [1] 3.88

``` r
print("Percent outliers left after filling")
```

    ## [1] "Percent outliers left after filling"

``` r
sum(is.na(data_outliers_filled)) / length(data_outliers_filled) * 100
```

    ## [1] 0

# Outlier Visualizations

``` r
dat_outliers <- data.frame(
    data=data_outliers, iqr=outliers_by_iqr, mad=outliers_by_mad,
    quantiles=outliers_by_quantiles, filled=data_outliers_filled)

iqrplt <- ggplot(data=dat_outliers) +
    geom_histogram(aes(x=data), fill="steelblue", alpha=0.4, bins=30) +
    geom_vline(
        colour="black",
        xintercept=(
            quantile(dat_outliers$data, 1/4, na.rm=TRUE) -
                1.5*IQR(dat_outliers$data, na.rm=TRUE))) +
    geom_vline(
        colour="black",
        xintercept=(
            quantile(dat_outliers$data, 3/4, na.rm=TRUE) +
                1.5*IQR(dat_outliers$data, na.rm=TRUE))) +
    labs(x="Data", y="Counts", title="Outliers by IQR")

madplt <- ggplot(data=dat_outliers) +
    geom_histogram(aes(x=data), fill="steelblue", alpha=0.4, bins=30) +
    geom_vline(
        colour="black",
        xintercept=(
            median(dat_outliers$data, na.rm=TRUE) -
                mad(dat_outliers$data, na.rm=TRUE) * 3
        )
    ) +
    geom_vline(
        colour="black",
        xintercept=(
            median(dat_outliers$data, na.rm=TRUE) +
                mad(dat_outliers$data, na.rm=TRUE) * 3
        )
    ) +
    labs(x="Data", y="Counts", title="Outliers by MAD")

quantileplt <- ggplot(data=dat_outliers) +
    geom_histogram(aes(x=data), fill="steelblue", alpha=0.4, bins=30) +
    geom_vline(
        colour="black",
        xintercept=quantile(dat_outliers$data, 0.02, na.rm=TRUE)) +
    geom_vline(
        colour="black",
        xintercept=quantile(dat_outliers$data, 0.98, na.rm=TRUE))+
    labs(x="Data", y="Counts", title="Outliers by Quantiles")

pltoutliers <- (iqrplt + madplt) / (quantileplt)
pltoutliers
```

    ## Warning: Removed 280 rows containing non-finite outside the scale range (`stat_bin()`).
    ## Removed 280 rows containing non-finite outside the scale range (`stat_bin()`).
    ## Removed 280 rows containing non-finite outside the scale range (`stat_bin()`).

![](OutiersImputed.jpeg)<!-- -->

``` r
ggsave("OutliersImputed.jpeg", pltoutliers, height=6, width=6)
```

    ## Warning: Removed 280 rows containing non-finite outside the scale range (`stat_bin()`).
    ## Removed 280 rows containing non-finite outside the scale range (`stat_bin()`).
    ## Removed 280 rows containing non-finite outside the scale range (`stat_bin()`).
