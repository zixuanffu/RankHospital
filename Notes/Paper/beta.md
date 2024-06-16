Section 1: Introduction

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
