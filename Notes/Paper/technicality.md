---
header-includes:
 - \usepackage{fvextra}
 - \DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines,commandchars=\\\{\}}
mainfont: Georgia
sansfont: Arial
monofont: Courier New
mathfont: Times New Roman

---

## Approximation

Assume that we are selecting the top $\alpha$ percent. 
$$
\theta_\alpha = G(1-\alpha)
$$
The local FDR is defined as 

\begin{align*}
\text{LFdr}_i= \frac{P(\theta_i<\theta_\alpha, \delta_i=1)}{P(\delta_i=1)}\\
= \frac{P(\theta_i<\theta_\alpha, \text{stat}_i>\lambda)}{P(\text{stat}_i>\lambda)*}
\end{align*}

which can be approximated by 

\begin{align*}
P(\theta_i<\theta_\alpha, \text{stat}_i>\lambda) = P(\theta_i<\theta_\alpha|\text{stat}_i>\lambda)P(\text{stat}_i>\lambda)\\
\approx P(\theta_i<\theta_\alpha|Y=y_i, S=s_i)P(\text{stat}_i>\lambda)\\
\approx \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda\}P(\theta_i<\theta_\alpha|Y=y_i, S=s_i)\\
= \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda\}(1-v_\alpha(y_i,s_i))
\end{align*}

or we can show the steps in this way

\begin{align*}
P(\theta_i<\theta_\alpha, \text{stat}_i>\lambda) \approx \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda, \theta_i<\theta_\alpha\}\\
\approx \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda\}P(\theta_i<\theta_\alpha|Y=y_i, S=s_i)\\
= \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i>\lambda\}(1-v_\alpha(y_i,s_i))
\end{align*}

The `ThreshFDR` function 
```r
function (lambda, stat, v)
{
    mean((1 - v) * (stat > lambda))/mean(stat > lambda)
}
```
To find the threshold $\lambda_1$
```r
lambda_1<-try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = RANKING_STAT, v = TAIL_PROB), silent = TRUE)
```
Both ways look sort of dubious to me...

## Right and left tail selection

### Rule: Posterior tail probability

|                   | Right                                                                                                                         | Left                                                                                                                          |
| ----------------- | ----------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| $\theta_\alpha$   | $G(1-\alpha)$                                                                                                                 | $G(\alpha)$                                                                                                                   |
| $v_\alpha(y)$     | $P(\theta_i>\theta_\alpha\|y)$                                                                                                | $P(\theta_i<\theta_\alpha\|y)$                                                                                                |
| cap constr approx | $\alpha= \frac{1}{n}\sum 1\{v_\alpha(y)>\lambda_2\}$                                                                          | $\alpha= \frac{1}{n}\sum 1\{v_\alpha(y)>\lambda_2\}$                                                                          |
| fdr constr approx | $\gamma = \frac{\frac{1}{n}\sum (1-v_\alpha(y_i))1\{v_\alpha(y_i)>\lambda_1\}}{\frac{1}{n}\sum 1\{v_\alpha(y_i)>\lambda_1\}}$ | $\gamma = \frac{\frac{1}{n}\sum (1-v_\alpha(y_i))1\{v_\alpha(y_i)>\lambda_1\}}{\frac{1}{n}\sum 1\{v_\alpha(y_i)>\lambda_1\}}$ |

Thus, for both left and right, we pick the max of $\lambda_1$ and $\lambda_2$ as the threshold for the two constraints.

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

Thus, for PM and MLE, the FDR is 
$$
\text{LFdr}_i= \frac{P(\theta_i>\theta_\alpha, \delta_i=1)}{P(\delta_i=1)}\\
= \frac{P(\theta_i>\theta_\alpha, \text{stat}_i<\lambda)}{P(\text{stat}_i<\lambda)}
$$
which is approximated by 
$$ 
 \frac{1}{n}\sum_{i=1}^n 1\{\text{stat}_i<\lambda\}(1-v_\alpha(y_i,s_i))
$$

To find the threshold $\lambda_1$
```r
lambda_1 <- - try(Finv(gamma, ThreshFDR, interval = c(0.1, 0.9), stat = -RANKING_STAT, v = TAIL_PROB), silent = TRUE)
#  minus sign
```
because $stat<\lambda$ is equivalent to $-stat>-\lambda$
