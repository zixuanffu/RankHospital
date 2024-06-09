# Panel data estimation

$$
y_{it} = x_{it} \beta + z_{it} \gamma + \theta_i + \epsilon_{it}
$$

## Standard errors

### Estimation

If we have homoskedastici error term $\epsilon_{it}$, then the WG estimation is consistent and efficient. Otherwise, first difference and use GLS on the differenced data.

### Inference 

The view of WG as a dummy-variable least-squares regression may suggest using a traditional (cross-sectional) ‘White’-type variance formula to deal with heteroskedasticity.