## Summary statistics

irSummary <- function(pre, post, best) {
    y <- post - pre
    x <- best - pre
    IR <- (mean(post) - mean(pre))/(best - mean(pre)) * 100
    sigma <- 1/(length(y) * mean(x)^2) * (IR^2 * var(x) + var(y) - 2 * IR * cov(x, y)) * 100^1
    return(list(IR = IR, sigma = sigma, N = length(y)))
}
## IR estimate
ir <- function(pre, post, best) {
    y <- post - pre
    x <- best - pre
    IR <- (mean(post) - mean(pre))/(best - mean(pre)) * 100
    return(IR)
}
## SD ir
irSD <- function(pre, post, best) {
    y <- post - pre
    x <- best - pre
    IR <- (mean(post) - mean(pre))/(best - mean(pre)) * 100
    sigma <- 1/(length(y) * mean(x)^2) * (IR^2 * var(x) + var(y) - 2 * IR * cov(x, y)) * 100^1
    return(sqrt(sigma))
}



## Confidence Intervals
deltaConf <- function(pre, post, best) {
    y <- post - pre
    x <- best - pre
    IR <- (mean(post) - mean(pre))/(best - mean(pre))
    sigma <- 1/(length(y) * mean(x)^2) * (IR^2 * var(x) + var(y) - 2 * IR * cov(x, y))
    ci <- IR + c(-1, 1) * qnorm(0.975) * sqrt(sigma)
    return(c(IR, ci) * 100)
}


## Null-hypothesis test
irTest <- function(x, y) {
    zobs <- (x$IR - y$IR)/sqrt(x$sigma/x$N + y$sigma/y$N)
    pval <- 2 * pnorm(-abs(zobs))
    
    return(list(Zstat = zobs, Pval = pval))
}

## ES

es <- function(pre, post) {
    ES <- (mean(post) - mean(pre))/sd(pre)
    return(ES)
}

## SRM

srm <- function(pre, post) {
    SRM <- (mean(post) - mean(pre))/sd(post - pre)
    return(SRM)
}



## Trend test
irTrend <- function(x, sds, score = seq_along(x)) {
    method <- "Chi-squared Test for Trends"
    dname <- paste(deparse(substitute(x)))
    dname <- paste(dname, ",\n using scores:", paste(score, collapse = " "))
    x <- as.vector(x)
    score <- as.vector(score)
    w <- sds/x
    a <- anova(lm(x ~ score, weights = w))
    chisq <- a["score", "Sum Sq"]
    names(chisq) <- "X-squared"
    df <- c(df = 1)
    pval <- pchisq(chisq, 1, lower.tail = FALSE)
    rval <- list(statistic = chisq, parameter = df, p.value = as.numeric(pval), method = method, data.name = dname)
    class(rval) <- "htest"
    return(rval)
} 
