# Constraints

## False discovery rate (FDR)

We distinguish two types of FDR.
1. As in `Benjamini and Hochberg (1995)`, 
$$ 
\text{FDR} = E\left[\frac{\sum h_i=0, \delta_i=1 }{\sum \delta_i} \right]
$$
2. As in `GuKoenker2023`,
   $$
   \text{mFDR}=\frac{\sum E[h_i=0, \delta_i=1 ]}{\sum E[\delta_i]}\\
   = \frac{P(h_i=0, \delta_i=1)}{P(\delta_i=1)}
$$
This is based on the assumption that there's an underlying distribution $G(\theta)$ of $\theta_i$. 

## Family-wise error rate (FWER)
We define the rank of an invidivual as
$$ r_i = 
$$