

1. [Download the data](/Code/DownloadData.R) 
   1. A sheet containing the year, name and url
   2. Download the zip folder using the url
   3. Unzip the folder, read the csv file and save it as a data.table in rds
   4. For csv such as SYGEN, PSY from SAE and hd from HOSPIDIAGE.
2. [Extract variables](/Code/ExtractVar.R)
   1. Separately from year 2016-2022 and 2013-2015
   2. labor input
   3. capacity
   4. labor output: 8 types, some may have different nomenclature.
   5. control: casemix, etc.
   6. legal status: some may change legal status
3. [Combine variables](/Code/CombineVar.R)
   1. Separately for the two periods.
   2. Combine the variables to have total count of nurses, physicians, etc.
4. [Descriptive statistics](/Code/DescribeData.R)
   1. The number of hospitals each year, per legal status.
   2. The share of each output, per legal status.
   3. Medical stays and personnel, per legal status.
5. [Estimate labor (in)efficiency](/Code/RegResults.R)
   1. [Regression analysis in `fixest`](/Code/RegX_fixest.R)
      1. `add_log`, `add_l`, `add_d` 
      2. `reg_X <- function(data, varl, varr, control = "CASEMIX", cluster = "FI", method = "ols")`
      3. `plot_FE <- function(reg_res, cluster, dt_status, year_start, year_end, filename, format = "pdf")`
   2. [Regression analysis in `plm`](/Code/RegX_plm.R)
      1. `extract.plm <- function( model, include.rsquared = TRUE, include.adjrs = FALSE, include.nobs = TRUE, ...)`
      2. `extract.pggls`
   3. Pooling
      1. OLS
      2. IV (lagged output)
      3. OLS with legal status dummy
      4. IV with legal status dummy
   4. Separate
      1. Pooling with IV
   5. Hospital fixed effect `pdt_used <- dt_inf[, .(AN, FI, STJR, fitted, residual, FixedEffect, Res)]`
      1. within group (strong exogeneity) `pdt_used_wg`
      2. first difference (relaxed) 
      3. first difference gmm (weak instruments, imprecise estimate) `pdt_used_gmm`
      4. system gmm (more precise estimate but over identification test rejected) `pdt_used_gmm_sys`
6. Empirical Bayes and NPMLE of the prior (`library(REBayes)`).  
   1. For [homogenous variance](/Code/SelectX.R).
      1. `fit1d`: prepare data for NPMLE of $G$
      2. `Lfdr.GLmix_temp`:Given an estimated mixing distribution, G, Lfdr computes an estimated local false discovery rate at a specified set of points and threshold value cnull. The argument G can be specified as the fitted object from one of several possible fitting routines for nonparametric mixing distributions.
      3. `ThreshFDREM` & `lik`: For EM/JS, compute the FDR rate when setting treshold lambda = lam
      4. `REBayes::ThreshFDR`: Compute the FDR rate when setting treshold lambda = lam. 
          ```{r} 
           ThreshFDR<-function (lambda, stat, v){
           mean((1 - v) * (stat > lambda))/mean(stat > lambda)
           }
           ```
      5. `selectR1d` & `select1d`: right selection and both.
      6. `grid_select` & `level_plot`: plot the dots and the level curve that represents the selection threshold curve.
      7. `select_plot_1d`: input the fixed effect estimate with variance, output whether the hospital is selected or not.
   2. For [heterogenous variance](/Code/SelectX_GLVmix.R)
      1. `fit2d`
7. Estimate $G$
   1. 1d estimation of $G$. `GLmix
   2. 2d estimation of $G$. Either [GLVmix](/Code/GLVmix.R) or [WGLVmix](/Code/WGLVmix.R). Both assuming dependent heterogeneity. whilw `WTLVmix` and `WLVmix` assumes independent heterogeneity.
   3. [Plotting the (smoothed) estimation results](/Code/EstimateG_NPMLE.R) 
8. Simulation based on estimated $G$: [1d estimation of $G$](/Code/SelectionResults.R) [2d estimation of $G$](/Code/SelectionResults_GLVmix.R)
9. Compare the results of different selection criteria (tp, pm etc.) $\timtes$ different constraints (capacity and false discovery rate). The [code](/Code/RankStatistics.R) is a bit convoluted.
10. Rank and select all hospitals: [1d](/Code/ComparePlot.R) and [2d](/Code/ComparePlot_GLVmix.R)
11. [Map the selection results](/Code/MapSelectRes.R)
  