# Variables

## Output
The severity of cases is measured by the cost of treatment (observable but not obtained here). But cost is also affected by efficiency. Therefore, using cost weighted output may confound the measure of efficiency. (For the same case, less efficient hospitals incur higher costs. But once the ouput is inflated by their cost, it appears that they are not that inefficient.)

# Functions 

## Conditional factor demand function
In standard microeconomics, the profit maximization problem is
$$
\max_{\vec{y}} \sum k_i y_i - \sum p_i x_i \quad \text{subject to} \quad f_i(x_1, x_2, \ldots, x_n) = y_i
$$
where $p_i$ is the price of input $i$ and $f$ is the cost function.

The cost minimization problem is 
$$ 
\min_{\vec{x}} \sum p_i x_i \quad \text{subject to} \quad f_i(x_1, x_2, \ldots, x_n) = y_i
$$
Thus, the factor demand function/correspondence is 
$$
x_i = x_i(p_1, p_2, \ldots, p_n, y_1, y_2, \ldots, y_m)
$$
### Arguement 1

> We can remain agnostic as to the nature of the appropriate formula for the aggregation of outputs and use as many different products as desired.

### Argument 2

> When input prices have low variability. Conditional factor demand can be estimated without information on input prices. 

> Even if we add prices, due a lack of variability, the price parameters will be poorly estimated.

### Argument 3

> From $x_i = x_i(p_1, p_2, \ldots, p_n, y_1, y_2, \ldots, y_m)$, we do not need to observe a complete list of inputs. But we do need to observe all input prices (can be ignored if almost no variability) and all outputs. While in the production function, it is the other way around (need to observe all inputs )

> Since, in our case, output is more *observable* than input (because capital is not easily observed), this approach is preferred.

## Production function

The production function is 
$$
y = f(x_1, x_2, \ldots, x_n)
$$
where $y$ is the output and $x_i$ are the inputs.
Production function

## Relationship between the two functions

Results: 
1. Input demand can be estimated imposing cross equation restrictions on the parameters? But the structural interpretation of the model is unimportant.


# Estimation

## Endogeneity issue in input demand estimation

The most **reduced form**. 
We let $y_{it} = x_{it} \beta + z_{it} \gamma + u_{it}=x_{it} \beta + z_{it} \gamma +\epsilon_{it} + \theta_i$ where $y_{it}$ denotes the input demand, $x_{it}$ denotes the output. $z_{it}$ denotes the control, and $\theta_i$ denotes the fixed effect.  

### Case 1

The strict exogeneity assumption:
$$ 
E[u_{it}|x_{i1},\ldots, x_{iT},z_{i1},\dots,z_{iT}]=0
$$
Then OLS will do.

### Case 2

$$
E[u_{it}|x_{it},z_{it}]\neq 0
$$
Then OLS is biased and inconsistent.
But 
$$
E[u_{it}|x_{i{t-1}},z_{i{t-1}}]=0
$$
Then we can use lagged variables as instruments.
Instruementing $x_{it}$ with $x_{i{t-1}}$


### Case 3

The above two cases are reasonable if we have a reasonably large amount of controls such that $u_{it}$ is uncorrelated with $x_{it}$ given $z_{it}$. ***WHY?***

Otherwise, we would need to difference out the fixed effects.

The strict exogeneity assumption:
$$
E[\epsilon_{it}|x_{i1},\ldots, x_{iT},z_{i1},\dots,z_{iT},\theta_i]=0
$$
Then we can use fixed effect estimator (demean or first difference almost equivalent).

### Case 4

If the case 3 strict exogeneity assumption does not hold, then we need to apply the Arellano-Bond estimator.

$$ 
E[\epsilon_{it}|x_{i1},\ldots, x_{it-p},z_{i1},\dots,z_{it-p},\theta_i]=0
$$
Then we would need to difference out the fixed effect and use lagged levels. The set of (over identifying) sequential moment conditions are
$$
E[\begin{pmatrix} x_{it-p-1} \\ \vdots \\ x_{i1} \end{pmatrix} \Delta \epsilon_{it}] = 0
$$
for each $t=1+p,\ldots,T$. 


# Appendix

## Exogeneity in panel data
1. [strict exogeneity](https://dlm-econometrics.blogspot.com/2019/06/there-is-exogeneity-and-then-there-is.html).
2. 



