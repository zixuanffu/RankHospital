Abstract: (written after the introduction)

Section 1: Introduction

% Hook: 

It is almost of human nature to compare, rank and select. And competition, be it good or bad, emerges in the wake. As invidious as ranking and selection can be, in many cases it is one of the driving forces behind improvement in performance (e.g. natural selection in the history of evolution). The society is constantly constructing league table. It rewards the meritous and question or even punishes the unsatisfactory. The measure based on which rank is constructed ranges from teacher's evaluation to communities' mobility index as pioneered by \cite{}. 

The present article extends the practice to the health sectors. To be more specific, it studies the labor efficiency across all hospitals in France. By exploring a comprehensive database (SAE) of French hospitals, we first construct a measure of labor efficiency. Then based on the estimates, we compare the public and private hospitals by selecting the top-performing units. We borrow from the recent developments in Empirical Bayes method to achieve the comparison.  

***Conclusion Here***

The article bridges two fields of interests. 
One is on productivity analysis. The most popular methods are Data Development Analysis \cite{} and Stochastic Frontier Analysis \cite{}. Yet we abstract from both of them for the convenience of statistical inference. We use the simple method applied in \cite{CroisetGarybobo_2024} by estimating a conditional input demand function. To put it simply, we estimate a linear function of how much labor input is needed to produce a give list of outputs \footnote{We refer the audience to xxx for detailed reasons of adopting this approach.}. We only focus on the employment level of nurses for (reasons) \footnote{} in the specification.

The second area of interests is the Empirical Bayes Methods. The name "Empirical Bayes" is self-telling in the sense that Bayes implies a prior distribution while Empirical means empirically estimate the prior from the data. Details on how it can be of use in ranking and selection will be presented in the rest of the section. 

In \cite{}, the authors argue that public hospital is less efficient than private counterpart in the sense that it would need a smaller size of personnel if it were to use the input demand function of the private hospital, which is the main result of their counterfactuals. 

Having roughly replicated the results after doubling the length of the panel, the paper differentiates itself by including/adding the standard/classical panel data methods in demand function estimation, specifically the fixed-effect within-group estimation and GMM \cite{}. 

***A tiny bit about the literature of GMM*** 

Though the original focus of the panel data estimator is on the $\beta$ parameters. It also provides us with a noisy estimate of the underlying unobserved heterogeneity term denoted as $\theta_i$. (It's important to note that this heterogeneity
is not necessarily indicative of inefficiency). Now that we have set the stage for empirical bayes, it is time to bring about the prior distribution of $\theta_i$, denoted as $G_{\theta}$. If the
prior distribution $G$ is known, having observed an estimate
$\hat{\theta}_i$ of $\theta_i$, we can update our noisy estimate $\hat{\theta}_i$ using or
incorporating our knowledge of $G$.

The usefulness of a prior $G$ is further exemplified/highlighted in the ranking
and selection problem mentioned above, when the object of interests is the
noisy estimate of $\theta_i$. For example, in \cite{}, we are given the task of
selecting the top 20\% out of the population of $\alpha_i$, that is to say
selecting those $i$ whose $alpha_i>G^{-1}(0.8)$, while controlling for the
overall false discovery rate ($\frac{x}{y}$) at 5\%. In \cite{}, the authors
try to develop an optimal decision rule for the given task. To put it in the
language of optimization, they want to have a decision rule that optimizes the
performance of selection, equivalently, minimize the loss of selection
\begin{equation*}
    \delta^* = \min_{\delta} \text{Loss} \quad \text{subject to contstraints}
\end{equation*}
where the loss function can take different forms, for example the
expected number of total type 1 and 2 mistakes.

The task at hand falls naturally under the compound decision framework pioneered by \cite{} if we define the loss function in a way that takes into account the results of all the individual decisions $\delta(Y_i)$. For instance, summing all mistakes would be one aggregate/compound individual decision.

It is obvious that in order to impose the two stated constraints (capacity and FDR) in formulating the optimization problem, we need to know the prior distribution $G$. 

Despite the importance of the $G$, it does not fall from heaven. Therefore, Empirical Bayes methods come to the rescue, as its name suggests, we will have to empirically estimating the unknown prior \(G\)".

Often "empirically estimating \(G\)" is done with parametric assumption that \(G\) is normal. Notable use cases are found in \cite{} which aroused the ensuing/lasting interests on teacher evaluation and social mobility in communities. By assuming a Gaussian $G$, they shrink the estimated fixed effect linearly, thus giving the name "linear shrinkage". However, departure from normality may render the linear shrinkage rule as unhelpful. Thanks to the foundational work of \cite{} who has shown that non-parametric estimation is also feasible and consistent, it is preferable to relax the normality assumption and estimate the prior $G$ non-parametrically. In terms of computation method, \cite{} has formulated the non-parmetric estimation as a convex optimization problem. Compared to other popular methods such as EM algorithm \cite{}, recent advancements in convex optimization computation methods \cite{} has made the novel approach of \cite{} computationally more attractive. 

It is worth mentioning here that though a discrete $G$ with at most x atoms can be estimated using the REBayes package \cite{}, we are not free of imposing any assumptions, that is the distribution of estimate 
$
\hat{\alpha}_i|\alpha_i,\sigma_i^2.
$
To illustrate, in the case of the estimate of fixed effect, we have

\begin{align*}
\hat{\alpha}_i &=\frac{1}{T}\sum_t (y_{it}-x_{it}\hat{\beta})\\ &=\frac{1}{T}\sum(\alpha_i+\varepsilon_{it}+x_{it}(\beta-\hat{\beta}))\\ &\to^d \alpha_i+\frac{1}{T}\sum_t \varepsilon_{it}\\
\end{align*}

The asymptotic distribution follows from the consistency of $\hat{\beta}$ when
$N \to \infty$, a reasonable assumption in wide panels.

If we may boldly assume that the errors are $i.i.d.$ normally distributed for
each $i$
\begin{equation*}
    \varepsilon_{it} \sim N(0, \sigma_i^2)
\end{equation*}
Then a fixed/small $T$ won't do jeopardize/imperil xxx our argument too much since we do not need to invoke central
limit theorem to have
\begin{equation*}
    \hat{\alpha}_i\to^d N(\alpha_i,\sigma_i^2/T)
\end{equation*}
However without the normality assumption on the error terms, we have to resort to the
central limit theorem from the claim that $T\to \infty$, which may seem unrealistic for a wide panel ($N>>T$).

The rest of the paper is organized as follows. Section 2 briefly describes the
data and lays out the reduced form estimation of the input demand function,
treating the number of nurses as the dependent variable and a list of 9 output
measures as the regressors. Section 3 applies the classical panel data
estimators to the same specification, distinguishing between whether strict
exogeneity is assumed. In section 4, we introduce the compound decision
framework and specifically/xxx define the selection problem. Section 5 follows
with a comparison of the different selection outcome as a result of imposing
varying constraints and assumptions. In section 6, we try to draw preliminary
conclusion on the comparative performance of public and private hospitals.
Section 7 discusses potential issues and concludes.


A bit of history here and the standard methods. (但不要放第一段这不是重点)

Efficiency and performance analysis are popular. While it is always firms whose main objective is to maximize profit, efficiency analysis has been extended to numerous fields. In the public sectors, education program and hospitals are the two most considered/studies areas. In fact, the classical data envolopment analysis (DEA) was first introduced to measure the performance of education programs. Parametric and non parametric methods are both developed. (An overview of the methods)

Having a measure/index of efficiency, it is often a natural subsequent step to compare/rank/select those most efficient ones and investigate those that are less satisfactory. The action that is taken falls under the philosophy of compound decision. Because we are given a a collection of measure and want to perform a collective action. We care about the performacne of the collective action. Empiriical bayes is useful in this regard. 

We explored underexploited dataset SAE. A panel of hospital data in France. The dataset covers all hospitals in France, with both public and private. Following the instruction given in CroisetGarybobo's paper, we extended the dataset to include earlier and later periods. Currently, it covers the period from 2013 to 2022, with the year 2020 missing.

Though it seems that the focus of the paper is the efficiency measurement, we adopt a naive/reduced form approach in the estimation. Without assuming the production function/cost function. In fact, the paper's main contribution is to apply Empirical Bayes method to selection of meritous hospitals given an initial estimate/measure of labor employment efficiency. In our paper, we treat the unobserved heterogeneity as the measurement of the hospital's efficiency. Given an initial estimate of the unobserved $\alpha_i$ for all the hospitals, a prior distribution $G$ can be estimated non-parametrically.
The idea is that given an crude/initial estimate of tEmpirical bayes is named by the fact the prior distribution is estimated from the data. We rely on the contribution of Koenker and Mizera to non parametrically estimate the prior distribution. 

While previously in ranking, posterior mean (prior is $G$) are widely used with variations and modification depending on different assumptions, in the context of selection, GuKoenker proposes another criteria in decision making process, posterior tail probability. This criteria is not entirely new as it is closley linked to the literatrue of multiple testing. 


The paper is organized as follows. In section 2, we lay out the simple reduced form model and replicated some of the qualitative resutls presented in CroisetGarybobo's paper.  The counterfactual as well. In section 3, we take a bold step to reestimate assuming individual unobserved heterogeneity. We assume the regressors to strictly exogenous and relax the assumption by applying standard approaches in panel data (system GMM). In section 4, we apply the empirical bayes method to the noisy estimate of the unobserved heterogeneity, in an attempt to denoise the estimate. In section 5, we utilize several criteria to select the most efficient hospitals and compare the selection results under different rules and assumptions of variance. In section 6, we want to comapre selection of best performing hospitals with that of other methods of efficiency measurement, DEA and SFA. In section 7, we conclude and discuss the limiitation and future extension of our work (data, method, model, etc.)


Section 2: Data and estimation results. 

The dataset we use is SAE. It as publicly accessible without any authorization required. It is reported by the hospitals each year in the form of a questionnaire, with detailed information on the hospitals personnel, output, capacity. While their paper only covers period 2016-2019 as the earlier years require a more time consuming aggregation, we extend the panel from 2013 to 2022. The panel includes all hospitals in France. We distinguish hospital by their legal status, while the origianl dataset only distinguish between public, private for profit and private non profit, we also further pin down the CHU hospitals among themn (as of 2024, there are 32 hostpitals in France). In fact, the entities understudy are not at the level juridique establishment, which means one CHU hospital can have many more subsidiaries that are treated as separate entities in the dataset.

Some descriptive statistics. The histogram. The box plot etc.  (Put into appendix plz)
1. The number of nurses (decomposition to registered and assistant) for each type of hospitals. and over the years.
2. The share of output for each type of hospitals. and over the years.
3. Inpatient and outpatient stay and stay length
4. The size of the hospitals (number of beds and number of slots.)


As mentioned in introduction, most efficiency measurement/analysis study relies on the estimation of production function. Advances have been made in the field. Instead, it is argued that in the context of hospitals, output is more exogenously given and input respond to the level of output (The argument is a bit dubious). Yet it is possible to use lagged values as instruments. as lagged output is correlated with current output and it is not affected by the current disturbance term. In fact IV and OLS estimates are very close, refecting maybe a lack of endogeneity issue. 

The specification is as follows. 
The estimates are reported in the table. The regression results give us a first glance at the differences between different types (legal status) of the hospitals. An initial/preliminary takeaway is that CHU employs much more nurses. 

We refer the audience to the original paper for selection of varibables in the regression specification. 

The counterfactuals... 

Section 3: Individual unobserved heterogeneity

Given an almost exhaustive list of output, we run the pwtest for unobserved heterogeneity. suggestive of the presence of unobserved heterogeneity. We then proceed to estimate the model with individual unobserved heterogeneity. 

We keep the same specification but adding individual fixed effect. The specification is as follows.

Though, the between hospital variation contributes to the fixed effect estiamtion. We also notice change within hospitals over the years. 

First we assume regressors are strictly exogenous,however naive it may be. It helps to have a first glance at the estimation results.

Later, we relax the assumption and acknowledge the endogeneity issue. We refer to panel data literature (1991 1998 2000). The persistence/stationarity of the regressors impose both a problem and a solution. We apply the method by Anderson and Hsiao to just identified IV. 

The results based on two assumptions are reported in the table.

Section 4: Empirical Bayes

The reason why we resort to fixed effect model is that we are interested in the estimated fixed effect however noisy they can be. The value added model on teachers has long been the example where empirical bayes is applied. Here, we adapted the method to hospital "value added" whether the value is efficiency. 

Put the framework here. 

Copy and paste previous notes.


Section 5: Selection comparison
We focus on the comparison of three criteria. Tail probability, mean and MLE and parametric james stein shrinkage rule.
Except MLE, all other methods take into account the variance of the estimates but with different degree of penalty. 
Section 6: Comparison with DEA and SFA

In estimating the efficiency and selecting the best performaing/underperforming units, the convention is to apply data envelopment analysis (cite) or stochastic frontier analysis (cite). Though the two methods are different in nature, they target the individual units just as we do here. 

Section 7: Conclusion 

We have revisited two strands of literature. One focus on efficiency and the other attempts to harvest the recent acheivement in Empirical Bayes method. We first compare the efficiency of hospitals by pooling and later separate regressions. Yet they are done with a focus on the 4 types. Yet we are not contented with a rich panel for almost xxx hospitals without looking at the more granular level of mearsurement. Though, the method in which we obtained an inidividual efficiency measure is not structural or even naive. It contributes to the literature of the empirical bayes method. Here, empirical bayes is applied to our compound decision of selecting the best performers among the hospitals. We have shown that among the top 20%. xxx is CHU, xxx is private for profit, xxx is private non profit. CHU is a special entity as the name suggested as teaching and research occupies a large part of their mission/objective compared to all other types of hospitals. The reason why we didn't choose medical doctors to measure labor efficiency is that there was generally a shortage of medical doctors in public hospitals and the supply of doctors are more constrained than nurses (stickiness of the supply compared to other types of personnel).
As the specification is overly simple, we do not draw conclusion on the actual efficiency until a better model or a richer dataset is available (PMSI for the exhaustive list of patient statistics in France).
It may be of interests to apply empirical bayes to DEA. 


Appendix: 
descriptive statistics
regression results
selection results
comparison results
