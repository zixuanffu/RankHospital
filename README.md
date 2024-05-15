# Ranking of French health care institutions:  
## Hospital efficiency data and preliminary results:
See the preliminary results: [here](Notes/Summary/main.pdf)  
Remarks:
1. The panel starts from 2016 and ends in 2022, with 2020 missing (nothing recorded in the year of 2020). Though it is also possible to extend it to 2013 with a bit more effort since the data summary is not availble before 2016, I need to construct it manually, to be completed later. (Before 2013 the data collection is not consistent). **TO BE COMPLETED**
2. Since the fixed effect is multiplicative, two estimation methods are considered. 
   $$y_{it} =x_{it1}^{\beta_1}x_{it2}^{\beta_2}x_{it3}^{\beta_3}\theta_i\epsilon_{it}\\
         = f(x_{it};\beta)\theta_i\epsilon_{it}$$
   1. Pseudo Poisson 
   2. Log linear OLS 
3. Since there may be endogeneity issue with the RHS variables (output such as short term acute care (STAC) inpatient stays and STAC outpatient stays), IV using one year lagged value can be considered. (Yet not implemented for Poisson method. **TO BE COMPLETED**)
4. Currently, the LHS is only the full time equivalent registered nurses. The RHS is STAC_inpatient, STAC_outpatient, Sessions. The RHS control is CASEMIX index provided by Hospidiag. *There is issue with zero values in some of the inputs. therefore removing observations with zero value in any one of the inputs. Observations drastically reduced.* **TO BE SOLVED**
5. The fixed effect can be added to the establishment level (FI, aka geographical level, more granular than FI_EJ) or the legal level (FI_EJ). for the figures produced, it is at the FI level. **TO BE DECIDED** **SUPER PRELIMINARY RESULTS**
![log linear OLS estimates, FE on each FI](https://github.com/zfuak/RankHospital/blob/5971e027906082eec530f1605fc66d01abc52fe0/Figures/2016-2022/FE_ols_FI.png)
![log linear OLS estimates, exp(FE) on each FI](https://github.com/zfuak/RankHospital/blob/5971e027906082eec530f1605fc66d01abc52fe0/Figures/2016-2022/FE_ols_FI_e.png)

## Ranking and selection based on Empirical Bayes (borrowing strength from the ensemble)
