#$
# Transform Functions
# Keith Chamberlain
##


score <- function(y, fun=mean, spread_agg=sum, power=2, var_power=1/2, ...){
    '
    calculate a transform of y.
    - z-score: fun=mean, spread_agg=sum, power=2, and var_power is 1/2
    - median-score: fun=median, spread_agg=sum, power=1, var_power=1
    '
    (y-fun(y, ...))/(
        spread_agg(abs(y-fun(y, ...))^power)/(length(y)-1))^var_power
    # Example:
    # dfs <- data.frame(
    #     data=rnorm(n=1000, mean=3, sd=5),
    #     x=1:1000
    # )
    # dfs$score <- score(y=dfs$data, fun=median, spread_agg=sum,
    #                    power=1, var_power=1)
    # dfs$ol <- !(dfs$score < 3 & dfs$score > -3)
    # plot(dfs$data~dfs$x, col=ifelse(dfs$ol, "darkred", "blue"))
}
