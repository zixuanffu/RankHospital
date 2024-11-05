
# Feedback
1. Christian: Why conditional input demand? 
   1. When input prices have low variability. Conditional factor demand can be estimated without information on input prices.
   2. We need to observe all outputs (which is our case).
   3. Be clearer on what the outcome measure is and its interpretation
2. Why not production function?
   1. We need to find a way to aggregate the output.  
   2. We need to observe all inputs. Capital is not easily observed.
3. Nour: Are the $\theta_i$ independent of each other? Yes, they are drawn independently from a distribution $G$. The estimate of $\hat{\theta}_i$, the correlation between each other i. Koen:  Under the null that $\theta_i$ are drawn independently from a distribution $G$, the correlation between $\hat{\theta}_i$ will go to zero as $N \to \infty$ (?)
4. Koen: You don't care about $\beta$. You can just regress $\log(x_it)$ on the fixed effect!!! If RHS regressors are insignificant -> may want to use FE location-scale model 
5. Arnaud: Control for the quality of nurses is important. You may want to get it from a survey for example. What is the selection of nurses into hospitals and how does that influence your estimation of theta? Are there other (unobserved) drivers of hospital quality and how does that interact with efficiency? -> structural model or at least some anecdotal evidence 
6. Panel data estimation: Issue with Sargan test.
7. Nour: you many want to check the Newey Smith paper on Higher order asymptotics of GMM
8. What is the asymptotic regime you are using? 
9.  Nour: What weight matrix is your GMM using? Are you using the optimal one? 
10. Paul: Why estimate in steps and not jointly? What is the reason behind this? 
11. Eric: For the inifinite dim. optimization problem: Computation of this might be NP-hard so you might want to give the intuition of how the method you are using deals with this issue. How does the NPMLE solves the infinite dimensional optimization problem?
12. Eric: Are there people doing deconvolution and how is it compared to the NPMLE?
13. Nour: Why not just select from the top 20%? Is the selected number smaller than 20% or more？
14. Olivier: For every assumption that you make, you may want to have some structural interpretation.
I think I really need to work on asymptotic analysis. I am almost completely oblivious to the literature on this. 
Also, on literature about inference.