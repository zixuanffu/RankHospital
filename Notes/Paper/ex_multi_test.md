# Example: multiple testing

## Introduction
The first compound decision is to estimate the **entire** vector $\alpha = (\alpha_1,\ldots,\alpha_m)$ where posterior mean decision rule is the best among the class of separable estimators. 

Now we introduce another compound decision problem: **multiple testing**.
We want to test for all $j$ if $\alpha_j \in A$.  

Consider only the separable class of decision rules $\delta(Y_j)$.
The decision rule $\delta(Y_j)$ gives a binary decision for each $j$.
$$
h(Y_j) = 1{\{\alpha_j \notin A\}} \\ 
\delta(Y_j) = 1\ \text{or}\ 0
$$

## Loss function

In order to find the **optimal decision rule**, we need to formulate the problem by defining the loss. 
$$
L(\alpha,\delta) = \sum_j \lambda 1{\{h_i=0, \delta_i=1\}} + 1{\{h_i=1, \delta_i=0\}} 
$$
where $\lambda$ is the loss of a false positive.

The risk (expected loss) is 
$$ E[L(\theta,\delta)] =\sum (\int_{h_i=0} \int \lambda \delta(y) p(y|\theta)dydG_\theta+\int_{h_i=1}\int (1-\delta(y))p(y|\theta)dydG_\theta)
$$
Recall that $h_i=0$ is equivalent to $\theta \in A$. Then the expected loss for obersvation $i$ is 
$$ 
\int_{\theta \in A} \int \lambda \delta(y) p(y|\theta)dydG_\theta-\int_{\theta\notin A}\int \delta(y)p(y|\theta)dydG_\theta+\int_{\theta \notin A}\int 1 p(y|\theta)dydG_\theta$$
$$
= A-B+\int_{\theta \notin A}dG_\theta
$$

## Optimal decision rule

The optimal decision rule is
$$
\delta(y_i) = 1\{P(\theta_i \notin A|y_i)>\frac{\lambda}{1+\lambda}\}
$$
where $P(\theta_i \notin A|y_i)$ is the posterior probability that $\theta_i$ is not in $A$.

The penalty/threshold term $\lambda$can be set either 
1. according to how we value the cost of the false positive vs the false negative,
2. according to the **capacity constraint** (how many we want to select)
3. according to other constraints such as the **local false discovery rate** or other types of false discovery rate (family-wise error rate, see appendix).

# Appendix
## Family-wise error rate