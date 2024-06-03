
## Approximation
Assume that we are selecting the top $\alpha$ percent. 
$$
\theta_\alpha = G(1-\alpha)
$$
The local FDR is defined as 
$$
\text{LFdr}_i= \frac{P(\theta_i<\theta_\alpha, \delta_i=1)}{P(\delta_i=1)}\\
= \frac{P(\theta_i<\theta_\alpha, \text{stat}_i>\lambda)}{P(\text{stat}_i>\lambda)}
$$
which can be approximated by 
$$ 
P(\theta_i<\theta_\alpha, \text{stat}_i>\lambda) = P(\theta_i<\theta_\alpha|\text{stat}_i>\lambda)P(\text{stat}_i>\lambda)\\
\approx P(\theta_i<\theta_\alpha|Y=y_i, S=s_i)P(\text{stat}_i>\lambda)\\
\approx \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda\}P(\theta_i<\theta_\alpha|Y=y_i, S=s_i)\\
= \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda\}(1-v_\alpha(y_i,s_i))
$$
or we can show the steps in this way
$$
P(\theta_i<\theta_\alpha, \text{stat}_i>\lambda) \approx \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda, \theta_i<\theta_\alpha\}\\
\approx \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda\}P(\theta_i<\theta_\alpha|Y=y_i, S=s_i)\\
= \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda\}(1-v_\alpha(y_i,s_i))
$$
Both ways look sort of dubious to me...
## Right and left tail selection

### Rule: Posterior tail probability

|                   | Right                                                                                                                         | Left                                                                                                                          |
| ----------------- | ----------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| $\theta_\alpha$   | $G(1-\alpha)$                                                                                                                 | $G(\alpha)$                                                                                                                   |
| $v_\alpha(y)$     | $P(\theta_i>\theta_\alpha\|y)$                                                                                                | $P(\theta_i<\theta_\alpha\|y)$                                                                                                |
| cap constr approx | $\alpha= \frac{1}{n}\sum 1\{v_\alpha(y)>\lambda_2\}$                                                                          | $\alpha= \frac{1}{n}\sum 1\{v_\alpha(y)>\lambda_2\}$                                                                          |
| fdr constr approx | $\gamma = \frac{\frac{1}{n}\sum (1-v_\alpha(y_i))1\{v_\alpha(y_i)>\lambda_1\}}{\frac{1}{n}\sum 1\{v_\alpha(y_i)>\lambda_1\}}$ | $\gamma = \frac{\frac{1}{n}\sum (1-v_\alpha(y_i))1\{v_\alpha(y_i)>\lambda_1\}}{\frac{1}{n}\sum 1\{v_\alpha(y_i)>\lambda_1\}}$ |

### Rule : Posterior mean
|                   | Right                                                                                                           | Left                                                                                                            |
| ----------------- | --------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| $\theta_\alpha$   | $G(1-\alpha)$                                                                                                   | $G(\alpha)$                                                                                                     |
| $v_\alpha(y)$     | $P(\theta_i>\theta_\alpha\|y)$                                                                                  | $P(\theta_i<\theta_\alpha\|y)$                                                                                  |
| $T(y)$            | $E(\theta_i\|y)=y+\frac{f(y)}{f'(y)}$                                                                           | ~                                                                                                               |
| cap constr approx | $\alpha= \frac{1}{n}\sum 1\{T(y_i)>\lambda_2\}$                                                                 | $\alpha= \frac{1}{n}\sum 1\{T(y_i)<\lambda_2\}$                                                                 |
| fdr constr approx | $\gamma = \frac{\frac{1}{n}\sum (1-v_\alpha(y_i))1\{T(y_i)>\lambda_1\}}{\frac{1}{n}\sum 1\{T(y_i)>\lambda_1\}}$ | $\gamma = \frac{\frac{1}{n}\sum (1-v_\alpha(y_i))1\{T(y_i)<\lambda_1\}}{\frac{1}{n}\sum 1\{T(y_i)<\lambda_1\}}$ |

### MLE and James-Stein rule
Same to the posterior mean rule, but with different $T(y)$