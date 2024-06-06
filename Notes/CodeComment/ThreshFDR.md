The two most used functions in the selection.
# Known variance
The followings are simplified expressions of the constraint.
## Calculate the posterior tail probability
$$
v_\alpha(y_i,\sigma_i) = P(\theta_i>\theta_\alpha|y_i,\sigma_i) = \frac{\int_{\theta_\alpha}^{\infty}f(y_i|\theta,\sigma_i)dG(\theta)}{\int_{-\infty}^{\infty}f(y_i|\theta,\sigma_i)dG(\theta)}
$$

```r
Lfdr.GLmix_temp
function(x, G, s, cnull, tail = "R") {
    # Modified for outliers??
    #' @Description: Given an estimated mixing distribution, G, mixing distributions. Calculate the posterior tail probability given the observed data x.
    #' @param x: a vector of estimates \hat{\theta} each following N(\theta_i, \sigma_i^2)
    #' @param G: the estimated distribution of $\theta_i$
    #' @param s: a vector of \sigma_i
    #' @param cnull: the threshold value


    v <- G$x # the grid points
    fv <- G$y # the density values
    A <- dnorm(outer(x, v, "-"), sd = s)
    # generate a matrix A with dim = (length(x), length(v))
    # at each point (i,j), give the density function of a normal distribution N(0, s[i]^2) evaluated at x[i] - v[j]
    # because at each point (i,j) we have the assumption x[i] ~ N(v[j], s[i]^2)
    A <- dnorm(outer(x, v, "-"), sd = s)
    if (tail == "R") {
        # the denominator is the probability of observing x[i] given the estimated distribution G
        # the nominator is the probability of observing x[i] AND its true mean   
        # 1- the ratio is the conditional probability of having the true mean value smaller than cnull given the observed value x[i] P(\theta_i < cnull | x[i])      
        v <- 1 - c((A %*% (fv * (v < cnull))) / (A %*% fv))
    } else {
        # 1- the ratio is the conditional probability of having the true mean value larger than cnull given the observed value x[i] P(\theta_i > cnull | x[i])
        v <- 1 - c((A %*% (fv * (v >= cnull))) / (A %*% fv))
    }
}
```
## Calculate the local false discovery rate
$$
\text{LFdr}=\frac{\frac{1}{n}\sum (1-v(y_i))1\{\delta_i=1\}}{\frac{1}{n}\sum 1\{\delta_i=1\}}
$$
```r
ThreshFDR 
function (lambda, stat, v)
{
    mean((1 - v) * (stat > lambda))/mean(stat > lambda)
}
```

# Compare $v_{\alpha}$ and $v_{\text{null}}$
The following functions are the original expression for the constraint presented in the `GuKoenker2023` before section 6.
## Calculate the posterior tail probability
```r
# calcaulte the tail probablity given the cutoff cnull
Lfdr <- function(x, G, s, cnull) {
    # for a given x, calculate v_\alpha(x, \sigma_i)
    v <- G$x
    fv <- G$y
    A <- dnorm(outer(x, v, "-"), sd = s)
    if (sum(A) == 0) {
        1
    } else {
        1 - c((A %*% (fv * (v < cnull))) / (A %*% fv))
    }
}
```
## Calculate the capacity and fdr threshold
```r
# ---- generate the capacity control level given a threshold $\lambda$ ---- #
Thresh0 <- function(lam, Fun, G, s, fs, domain, cnull) {
    ns <- length(s)
    nu <- rep(NA, ns)
    for (j in 1:ns) {
        cut <- Finv(lam, Fun, interval = domain, G = G, s = s[j], cnull)
        nu[j] <- sum(G$y * (1 - pnorm(cut - G$x, sd = s[j])))
    }
    crossprod(nu, fs)
}
# ---- generate the fdr level given a threshold $\lambda$ ---- #
Thresh1 <- function(lam, Fun, G, s, fs, domain, cnull, znull) {
    # find value such that Fun criterion is met
    ns <- length(s)
    nu <- rep(NA, ns)
    cnu <- rep(NA, ns)
    # if(cnull == 4) browser()
    for (j in 1:ns) {
        cut <- Finv(lam, Fun, interval = domain, G = G, s = s[j], cnull)
        nu[j] <- sum(G$y * (1 - pnorm(cut - G$x, sd = s[j])))
        cnu[j] <- sum(G$y * (G$x < znull) * (1 - pnorm(cut - G$x, sd = s[j])))
    }
    crossprod(cnu, fs) / crossprod(nu, fs)
}
```

### solve for cutoff given threshold
For each $\sigma_i$, using the posterior tail probability formula  `LFdr` $v_{\alpha}(y_i,\sigma_i)$, given $\lambda$ solve for cutoff
$$
v_\alpha(cut, \sigma_i) = \lambda
$$
```r
cut <- Finv(lam, Fun, interval = domain, G = G, s = s[j], cnull)
```
#### Given cutoff, calculate capacity rate

+ First, we are taking integral
$$\int 1-\Phi((cut-\theta)/\sigma_i) dG(\theta)
$$
```r
nu[j] <- sum(G$y * (1 - pnorm(cut - G$x, sd = s[j])))
```
+ Next. we are taking another integral
$$
\int \int 1-\Phi((cut-\theta)/\sigma_i) dG(\theta)dH(\sigma_i)
$$
```r
crossprod(nu, fs)
```
+ In the end, we get the capacity given the input threshold $\lambda$.

Same idea for fdr, we will get fdr given the input threshold. 




