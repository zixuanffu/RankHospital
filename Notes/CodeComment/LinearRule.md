# Linear rule 

Following the discussion in `Walter2022nberslides`, we have the hierarchical model.
$$
\hat{\theta}_i|\theta_i,\sigma_i \sim N(\theta_i,\sigma_i^2)
$$
And 
$$
\theta_i |\sigma_i^2 \sim N(\mu_{\theta},\sigma_{\theta}^2)
$$
The $\mu_{\theta}$ and $\sigma_{\theta}$ are the hyperparameters.
With this normal prior, deconvolution of G only requires estiamting the two hyperparameters.  
The common estimators for the hyperparameters are the following.
$$
\hat{\mu}_{\theta} = \frac{1}{N}\sum \hat{\theta}_i
$$
And
$$
\hat{\sigma}_{\theta}^2 = \frac{1}{N}\sum [(\hat{\theta}_i - \hat{\mu}_{\theta})^2-\hat{\sigma}_i^2]
$$
note: subtracting the variance of the observed data is a bias correction term accounting for excess variance in $\hat{\theta}_i$ due to sampling error.
> $\hat{\sigma}_{\theta}^2<0$ implies overdispesion beyond what we'd expect from noise.

Or alternatively, we can use the maximum likelihood estimator to estimate the hyperparameters.
The function to minimize (resulted from maximum likelihood) to is the following.
$$
\sum \log(\sigma_\theta + s_i^2) + \sum \frac{(x_i - \mu_\theta)^2}{\sigma_\theta+ s_i^2}
$$

```r
lik <- function(theta, x, s) {
    sum(log(theta[2] + s^2)) + sum((x - theta[1])^2 / (theta[2] + s^2))
}
```