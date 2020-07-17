Causal Analysis - RHC Dataset
================
July 16th, 2020

## Causal Analysis

This is a class exercise on Causal Analysis training from Coursera’s
[Crash Course in
Causality](http://www.coursera.org/learn/crash-course-in-causality) -
Week 3 (Matching).

### RHC Dataset

This dataset was used in Connors et al. (1996): The effectiveness of RHC
in the initial care of critically ill patients. J American Medical
Association 276:889-897. The dataset pertains to day 1 of
hospitalization, i.e., the “treatment” variable swang1 is whether or not
a patient received a RHC (also called the Swan-Ganz catheter) on the
first day in which the patient qualified for the SUPPORT study (see
above). The dataset is suitable for use in papers submitted in response
to the call for papers on causal inference, by the journal Health
Services and Outcomes Research Methodology. The original analysis by
Connors et al. used binary logistic model to develop a propensity score
that was then used for matching RHC patients with non-RHC patients. A
sensitivity analysis was also done. The results provided some evidence
that patients receiving RHC had decreased survival time, and the
sensitivity analysis indicated that any unmeasured confounder would have
to be somewhat strong to explain away the results. See Lin DY, Psaty BM,
Kronmal RA (1998): Assessing the sensitivity of regression results to
unmeasured confounders in observational studies. Biometrics 54:948-963
for useful methods for sensitivity analysis, one of which was applied to
the RHC results.

This data was extracted from [this
website](http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets).

### Loading the Data

``` r
# Libraries
library(tableone)
library(Matching)

# Loading the data
load(url("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.sav"))
```

### Preparing the dataset

The data is highly coded and we wanted to filter some columns and codify
some covariates as 0 or 1’s.

``` r
ARF = as.numeric(rhc$cat1 == "ARF")
CHF = as.numeric(rhc$cat1 == "CHF")
Cirr = as.numeric(rhc$cat1 == "Cirrhosis")
colcan = as.numeric(rhc$cat1 == "Colon Cancer")
Coma = as.numeric(rhc$cat1 == "Coma")
COPD = as.numeric(rhc$cat1 == "COPD")
lungcan = as.numeric(rhc$cat1 == "Lung Cancer")
MOSF = as.numeric(rhc$cat1 == "MOSF w/Malignancy")
sepsis = as.numeric(rhc$cat1 == "MOSF w/Sepsis")
female = as.numeric(rhc$sex == "Female")
died = as.numeric(rhc$death == "Yes")
age = rhc$age
treatment = as.numeric(rhc$swang1 == "RHC")
meanbp1 <- rhc$meanbp1

mydata = data.frame(
  cbind(ARF, CHF, Cirr, colcan, Coma, lungcan, MOSF, sepsis, age, female, meanbp1, treatment, died)
)
xvars = colnames(mydata)[1:11]

head(mydata)
```

    ##   ARF CHF Cirr colcan Coma lungcan MOSF sepsis      age female meanbp1
    ## 1   0   0    0      0    0       0    0      0 70.25098      0      41
    ## 2   0   0    0      0    0       0    0      1 78.17896      1      63
    ## 3   0   0    0      0    0       0    1      0 46.09198      1      57
    ## 4   1   0    0      0    0       0    0      0 75.33197      1      55
    ## 5   0   0    0      0    0       0    0      1 67.90997      0      65
    ## 6   0   0    0      0    0       0    0      0 86.07794      1     115
    ##   treatment died
    ## 1         0    0
    ## 2         1    1
    ## 3         1    0
    ## 4         0    1
    ## 5         1    1
    ## 6         0    0

### Pre-matching

Now that the data is ready to use, let’s check how balanced it is,
before the matching exercise:

``` r
table1 = CreateTableOne(vars = xvars, strata = "treatment", data = mydata, test = F)
print(table1, smd = T)
```

    ##                      Stratified by treatment
    ##                       0             1             SMD   
    ##   n                    3551          2184               
    ##   ARF (mean (SD))      0.45 (0.50)   0.42 (0.49)   0.059
    ##   CHF (mean (SD))      0.07 (0.25)   0.10 (0.29)   0.095
    ##   Cirr (mean (SD))     0.05 (0.22)   0.02 (0.15)   0.145
    ##   colcan (mean (SD))   0.00 (0.04)   0.00 (0.02)   0.038
    ##   Coma (mean (SD))     0.10 (0.29)   0.04 (0.20)   0.207
    ##   lungcan (mean (SD))  0.01 (0.10)   0.00 (0.05)   0.095
    ##   MOSF (mean (SD))     0.07 (0.25)   0.07 (0.26)   0.018
    ##   sepsis (mean (SD))   0.15 (0.36)   0.32 (0.47)   0.415
    ##   age (mean (SD))     61.76 (17.29) 60.75 (15.63)  0.061
    ##   female (mean (SD))   0.46 (0.50)   0.41 (0.49)   0.093
    ##   meanbp1 (mean (SD)) 84.87 (38.87) 68.20 (34.24)  0.455

You can see many columns with unbalances, which means SMD \> 0.1, such
as: Cirr, Coma, sepsis, etc. To solve for those unbalances, we need to
match the data.

### Greedy Match

``` r
greedymatch = Match(Tr = treatment, M = 1, X = mydata[xvars])
# M = 1 to get one match per treatment subject

# Then we extract from the output the index for treated and control subjects
matched = mydata[unlist(greedymatch[c("index.treated","index.control")]),]

# Then let's check for unbalances again
matchedtab1 = CreateTableOne(vars = xvars, strata = "treatment", data = matched, test = F)
print(matchedtab1, smd = T)
```

    ##                      Stratified by treatment
    ##                       0             1             SMD   
    ##   n                    2186          2186               
    ##   ARF (mean (SD))      0.42 (0.49)   0.42 (0.49)  <0.001
    ##   CHF (mean (SD))      0.10 (0.29)   0.10 (0.29)  <0.001
    ##   Cirr (mean (SD))     0.02 (0.15)   0.02 (0.15)  <0.001
    ##   colcan (mean (SD))   0.00 (0.02)   0.00 (0.02)  <0.001
    ##   Coma (mean (SD))     0.04 (0.20)   0.04 (0.20)  <0.001
    ##   lungcan (mean (SD))  0.00 (0.05)   0.00 (0.05)  <0.001
    ##   MOSF (mean (SD))     0.07 (0.26)   0.07 (0.26)  <0.001
    ##   sepsis (mean (SD))   0.32 (0.47)   0.32 (0.47)  <0.001
    ##   age (mean (SD))     60.84 (15.54) 60.77 (15.64)  0.005
    ##   female (mean (SD))   0.41 (0.49)   0.41 (0.49)  <0.001
    ##   meanbp1 (mean (SD)) 68.26 (33.23) 68.19 (34.23)  0.002

Now we have a very good balance, given all covariates SMD \< 0.1.

### Outcome

Now that we have a balanced matched set of observations, let’s proceed
with outcome analysis. In this case we have treatment x control and the
outcome variable is died.

``` r
y_trt = matched$died[matched$treatment == 1]
y_con = matched$died[matched$treatment == 0]

# Pairwise the difference
diffy = y_trt - y_con

# Paried test
t.test(diffy)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  diffy
    ## t = 3.3318, df = 2185, p-value = 0.0008773
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  0.01863193 0.07194447
    ## sample estimates:
    ## mean of x 
    ## 0.0452882

So, as a summary:

  - Point estimate (of difference): **0.045**
      - Difference in probability of death if everyone received RHC
        versus if no one received RHC is 0.045 (higher risk of death in
        RHC group).
  - 95% Confidence Interval: **(0.019, 0.072)**
  - p-value **\< 0.001**
