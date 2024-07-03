
# Opening
Thank you for coming to my presentation. The subject I am going to present today is an application of the Empirical Bayes method in selecting the most labor efficient hospitals. Thus, giving the name "Hopital selection rule". PUN INTENDED.

# Introduction
My presentation connects two fields of interests. The first one is on productivity analysis. My goal is to use efficiency analysis to draw a comparison between the public and private hospitals in france. Due to the constraint of data, I only focus on the use of labor, employment efficiency and abstract from quality and other measure etc. 
Instead of estimating a production function, such as the stochastic frontier model or use a non parametric approach such as data envelopment, I follow \cite{croiset2024} and use a simple specification that can have the interpretation of **conditional demand function**. So the reduced form relationship is that the dependent variable is the number of labor input, and the right-hand side is a list of medical outputs. Under this specification, holding the same amount of output, larger y means less efficient.

The second strand of literature is the main focus of today's exposition. As pointed out by \cite{Gu2023}. It is of human nature to rank and select. This tendency is given a name league table mentality. However, unfortunately, most of the time, what we have is the noisy estimate of the things we want to rank or select. The estimate of fixed effect from a fixed effect/panel data estimation. Secondly, I care about the collective performance of my decision, not just one single individual decision, giving the name to **compound** decision. This is when empirical Bayes method can come to the rescue. When I say Bayes, that is to say, I want to assume a prior distribution of the true parameters, the parameters I care about, want to rank or select. Then estimate it empirically the prior. Using parametric assumption, or as applied in this presentation, NPMLE. 

Having surveyed/gone through the two strands of literature, the simple/question and the easy exercise that I want to do is making comparison between the public and private hospitals from a granular perspective. Out of the around 1600 hospitals in the sample, I want to select the top 20% percent, the best performing units. I classify them by legal status and see how many are public and how many are private. Of course, this can be done simply by running a fixed effect regression, take the estimated fixed effect and select the best units by the "face value". However, as mentioned, this is a noisy estimate. In order to be more careful, less naive in making this kind of decision, I want to control for the expected number of mistakes. How do I control it? What would be the selection outcome if I control for the mistakes? Besides, there are different ranking statistics that I can use to select, for example the face value, and many others, what would be the outcome if I use different ranking statistics and hold different assumptions. 

All in all, this presentation is about comparison. compare the private with public. compare different ranking statistics and assumptions. compare the outcome with and without constraint.

A quick recap. I need to first have an estimate of the hospital efficiency. In my case, since it is an input demand specification, the fixed effect is the inefficiency measure. The smaller the theta, the more efficient the hospital i is. Then I need to construct a prior distribution. Finally I can perform selection incorporating the information from $G$.
After having a quick look at the data, I will go directly to last 2 steps, then the estimation and issues I encounter with estimation. 

# Data
In terms of gathering data, I follow the paper by croiset and gary-bobo.
The data I have is from statistique annuelle d'etablissement. It is completely publically available. Covering all french hospitals from 2013 to 2022. However, year 2020 is completely missing. The number of hospitals is quite stable over the years. The data itself only distinguish public, private for profit and private non profit. However, it is important to single out the teaching hospitals from the public. Teaching hospitals are humongous hospitals that not only treat patient but also train medical students. They need to allocate a significant amount of resources to teaching and research, unlike the other types.

Turn to the output of each type of hospitals, weighted by the number of hospitals. We can see that each type of hospitals differ in the mix of services that they provide. Emergency care is mostly taken care of by
public hospitals and private hospitals are strong in medical sessions.
As can be reconfirmed, the teaching is quite different (large). and we will exclude them from estimation so that it is more reasonable to assume that all hospitals have similar input demand function.

#  Compound decision framework

Having an overview of what the data is about. Let's jump to the second part of paper. Selection as compound decision. Let's say we observe a vector of estimate of the true parameter $\theta$. Each estimate conditional on the true parameter follows a certain distribution of P. For example, a normal distribution or Poisson distribution. The delta is my decision based on the observed estimate. For example, in selection problem, delta is 1 or 0, indicating whether i is selected or not. included in the selection set or not.

The reason why this is a compound decision is simply from the way we define the selection problem. The way we define the loss function. The collective/compound loss function is the simple sum of individual loss. Therefore, the objective of the selection problem is to minimize the expected compound loss -- compound risk.
The compound decision framework is first coined by Robbins in 1950s. How is it related to bayesian view? The compound decision framework is more related to the fixed effect/frequentist view, by treating $\theta_i$ as fixed unknown parameter. $G_n$ is the empirical distribution of the true parameter. The bayes view assumes that the $\theta_i$ does follow a distribution $G$. So the compound decision framework is connected to bayesian view by replacing $G_n$ with $G$.

So we have defined our objective loss function. But the objective function depends on the prior distribution $G$. Unfortunately, we don't know $G$, thus we want to estimate $G$ using mle. The $g$ is the likelihood of observing $y$. Kiefer and Wolowitz have established the consistency of such a NP estimation. As can be easily shown that this is an infinite dimension convex optimization with linear constraints. The recent development in computation method helps solving the NPMLE under certain discretization.

# Selection task
Previously, I denote the loss by L. And this L depends on the specific problem we are trying to tackle. Having tackled the estimation of $G$, now we turn to define our specific selection problem. For a selection problem, the main objective is to select the bottom 20% of \theta_i, note that we want to select the true theta i not theta i hat. Also, we want to control the number of falsely selected. The expected number of wrongly selected/all selected. How do we define the loss function.
First, We want to minimize the number of $i$ that belongs to the set but unselected. h corresponds to whether the true value belongs to the set. \delta corresponds to whether unit i is selected. Second, it is the constraint that limits our interests to the bottom 20%. Third, it is the constraint that control the expected false discovery.

The loss function is explicitly defined, but since we don't know the true value of each \theta. We will take expectation of the loss. By taking expectation, we retrieve the most important factor of the compound risk posterior tail probability. This is the probability of being in the tail set given the estimate hat theta.

To put the question into context, let's say the true inefficiency parameter is theta_i, we observe a sequence of Y_it that follows a normal distribution centered at the true value. The worst case scenario is that we don't know theta nor the variance sigma. But we have two sufficient statistics for them. Y_i and S_i. with respective distribution. For example, y_it is the loginput-logouput times beta, but subject to assumptions that the error term in the input demand specification is normal.

So what is the ptp? The posterior tail p that theta belongs to the set given our estimate of theta_i and sigma_i. This is the bayes rule without any issue.

Now we have defined the ptp, back to our risk function. minimize the first term subject to two constraint. The first term is saying that we select one by one from the highest tail prob to the lowest. The constraints are saying we stop selecting once a constraint is hit. Therefore, there exists a cutoff \lambda such that we select all i whose tp is higher than lambda. And one constraint binds.

A recap.
1. We have a longitudinal panel. For each i, we got a T observation. Each observation Y_it is normal around the true theta_i.
2. Given the yit, we perform NPMLE to get a G. and smoothed it.
3. Given the estimated G, we calculate ptp for each i. as well as the two constraints.
4. Solve the problem and find the optimal cutoff lamda such that all i whose ptp higher than lambda is selected.

# Results
What would it be like in the hospital application?

Previously, I have stated the most general case where either theta_i nor sigma_i is known. An estimate gives me the LHS. In many of the literature reference, due to the difference in data and model, they assume that sigma is known. I also apply it by setting the estimated s_i as the true value, which gives me the RHS.

I can also smooth it by kernel smoothing. (biweight kernel with kernal width)

Let us look at the case we have been talking about, bottm 20% and FDR 20%. 
The private are x times more than public.
Imposing FDR cst shrinks the selected set by a dozen.
What if we take sigma as known? Then surprisingly, the FDR is not binding.
(Intuition)
This is not the same as taking the face value of theta hat and select. As we can see from the figure. For MLE, the cutoff is a staightline, while in TP, it is curved. 

# Estimation
WG FD estimation. First Difference GMM. But the overidentification is rejected. However when I assume that some regressors are exogenous but still overidentified moment condition, overidentification test is not rejected. Statistically I can do that, but it doesn't make economics sense to me why some regressors are exogenous while some are not... For future exploration.

# Conclusion

 From the application to French
hospitals, it is clear that the FDR constraint shrinks the selection set by
some amount. It is also intuitive that the larger the capacity (the larger the
$\alpha$), the less binding the FDR constraint is. The idea is that when the
decision-maker can select more units, the probability of making mistakes
decreases. Another observation comes from the assumption we make in the NPMLE
of $G$. In \citet{gu2023invidious}, the authors have pointed out that while the
known variance assumption in $Y_i|\theta_i,\sigma_i$ may be plausible in some
applications, it is more common to be faced with only an estimate of the
variance. The two assumptions give rise to a different level of
\textit{stringency} in response to the constraints, especially when the
decision-maker wants to control for the expected false discovery rate. Assuming
an unknown variance treats the observation as noisier, thus increasing the
probability of making mistakes. The same level of FDR constraint of 20\% only
binds in the unknown variance scenario. With respect to the private-public
comparison, among the top 20\% performers, there are around ten times more
private than public hospitals, while the ratio of total number is 5 to 3. A
preliminary conclusion is that in terms of labor employment efficiency, there
are more efficient private hospitals among the top performers. It may be of
interest to healthcare authorities to perform such selections and take
corresponding actions with respect to the selection outcome. From my point of
view, a related report based on the ranking and selection results will create
an incentive for healthcare providers to ensure the completeness of data input.
However, one cautionary note is the interpretation of the fixed effect
estimate. Since the fixed effect captures all time-invariant components of the
unit, whether it is only the unobserved heterogeneity of individual hospitals
or an actual measure of inefficiency is questionable. This issue is discussed
in \citet{greene2005fixed}. Lastly, despite the fact that it is human nature to
construct rankings and make selections, every step of the procedure requires
attention to specification, identification, and justifiable assumptions.
Incorporating constraints such as FDR in defining the problem may be helpful,
but the decision is still subject to great uncertainty and should be made with
caution and justification.


% Mumbling
It is of human nature to rank and select. There's this so-called league table mentality as pointed out in the paper by Gu and Koenker. (my main reference)
In the mean time, there's ongoing interests in the comparison between public and private hospitals. Given the dataset that I have, there's interests in Therefore, what I want to do is to be tempted by the allure of selection and to select the top performing 20% hospitals. Instead of taking my estimate at the face value, I want to borrow strength from the ensemble (borrow from the bayesian view). For example, since this is a selectio problem, i want to impose the constraint of how many to select. Moreover, I want to control the number of falsely selected. I also want to compare the selection outcome if i hold different assumptions about my estimates of fixed effect. 

To put it simply, the selection problem consists of two step in the big picture. The first part is on fixed effect estimation. Assuming my specification is right, I will get a noisy estimate of the individual fixed effect. In my context, the fixed effect is the inverse of efficiency. The smaller the fixed effect, the higher the efficiency. The second step is to construct a prior distribution of the fixed effect by NPMLE. This is why we call  it bayesian method. The prior distribution is then of use to defining the selection problem. Because my selection problem is a compound decision.

