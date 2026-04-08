# Comparing Models


[Source](https://bookdown.org/sarahwerth2024/CategoricalBook/review-comparing-models-r.html)

``` r
libraries <- list(
  "tidyverse", "car", "lmtest"
)
invisible(lapply(libraries, library, character.only = TRUE))
```

# Question and Prep

What characteristics are associated with separatist movements in
ethnocultural groups.

``` r
MAR <- read_csv("data/M.A.R_Cleaned.csv")
```

``` r
MAR <- MAR %>%
  mutate(
    trsntlkin = factor(trsntlkin, labels = c(
      "No Close Kindred",
      "Close Kindred/Not Adjoining Base",
      "Close Kindred, Adjoining Base",
      "Close Kindred, More than 1 Country"
    )),
    kinmatsup = factor(kinmatsup, labels = c(
      "No material kinship support",
      "Material kinship support"
    )),
    kinmilsup = factor(kinmilsup, labels = c(
      "No military kinship support",
      "Military kinship support"
    )),
    grplang = factor(grplang, labels = c(
      "<10% Speak Native Lang",
      "Some Speak Native Lang",
      "Most Speak Native Lang"
    ))
  )
```

# Wald Test

Does adding or removing sets of variables significantly change the fit.

Test hypotheses: - Null hypothesis: the coefficients are all equal to 0
($H_0: X1 = X2 = 0$) - Alt hypothesis: the coefficients are not all
equal to zero ($H_A: X1 \ or \ X2 \neq 0$)

Interpretation: If the p value for the test is \< .05, then we reject
the null hypothesis and the variables significantly explain some piece
of the model. It is like the p-value for our coefficients, except for
multiple coefficients at the same time.

## Run regression

``` r
fit1 <- glm(sepkin ~ trsntlkin + kinmatsup + kinmilsup +
              grplang + pct_ctry,
            data = MAR, family = binomial(link = "logit"))
summary(fit1, show.residuals = TRUE)
```


    Call:
    glm(formula = sepkin ~ trsntlkin + kinmatsup + kinmilsup + grplang + 
        pct_ctry, family = binomial(link = "logit"), data = MAR)

    Coefficients:
                                                  Estimate Std. Error z value
    (Intercept)                                 -3.0587629  0.4824304  -6.340
    trsntlkinClose Kindred/Not Adjoining Base    1.8558380  0.4394281   4.223
    trsntlkinClose Kindred, Adjoining Base       1.6448396  0.4544475   3.619
    trsntlkinClose Kindred, More than 1 Country  2.4308115  0.4495357   5.407
    kinmatsupMaterial kinship support           -0.2695913  0.3839679  -0.702
    kinmilsupMilitary kinship support            1.5463083  0.4640465   3.332
    grplangSome Speak Native Lang                0.2350165  0.2380001   0.987
    grplangMost Speak Native Lang                1.0265800  0.2753856   3.728
    pct_ctry                                    -0.0002825  0.0061351  -0.046
                                                Pr(>|z|)    
    (Intercept)                                 2.29e-10 ***
    trsntlkinClose Kindred/Not Adjoining Base   2.41e-05 ***
    trsntlkinClose Kindred, Adjoining Base      0.000295 ***
    trsntlkinClose Kindred, More than 1 Country 6.40e-08 ***
    kinmatsupMaterial kinship support           0.482605    
    kinmilsupMilitary kinship support           0.000862 ***
    grplangSome Speak Native Lang               0.323415    
    grplangMost Speak Native Lang               0.000193 ***
    pct_ctry                                    0.963279    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for binomial family taken to be 1)

        Null deviance: 1013.80  on 836  degrees of freedom
    Residual deviance:  926.81  on 828  degrees of freedom
      (15 observations deleted due to missingness)
    AIC: 944.81

    Number of Fisher Scoring iterations: 5

``` r
summary(resid(fit1, type = "deviance"))
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    -1.7868 -0.8022 -0.7235 -0.1160  1.2534  2.4011 

## `linearHypothesis`

1.  Material and military support from kin

For binary variables, are the coefficients for yes equal to 0?

``` r
linearHypothesis(fit1, c("kinmatsupMaterial kinship support = 0",
                          "kinmilsupMilitary kinship support = 0"))
```


    Linear hypothesis test:
    kinmatsupMaterial kinship support = 0
    kinmilsupMilitary kinship support = 0

    Model 1: restricted model
    Model 2: sepkin ~ trsntlkin + kinmatsup + kinmilsup + grplang + pct_ctry

      Res.Df Df  Chisq Pr(>Chisq)   
    1    830                        
    2    828  2 11.108   0.003871 **
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

2.  Single categorical variables with multiple levels

``` r
linearHypothesis(fit1, c("grplangSome Speak Native Lang",
                         "grplangMost Speak Native Lang"))
```


    Linear hypothesis test:
    grplangSome Speak Native Lang = 0
    grplangMost Speak Native Lang = 0

    Model 1: restricted model
    Model 2: sepkin ~ trsntlkin + kinmatsup + kinmilsup + grplang + pct_ctry

      Res.Df Df  Chisq Pr(>Chisq)    
    1    830                         
    2    828  2 18.999  7.489e-05 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Interpret

In both cases, the null hypothesis can be rejected, meaning the
variables improve the fit.

# Likelihood Ratio Test

Most recommended test for comparing glm models, including linear
regression, logistic regression, probit regression, ordinal models,
poisson, and negative binomial models.

## Overview

Likelihood refers to the probability that the relationship between
variables described in the model equation is the true relationship.
Maximum likelihood uses the log of the likelihood and compares model
fits. with the likelihood ratio test.

Test hypotheses:

- Null hypothesis: the log-likelihoods from one model is equal to the
  other (H0: log-likelihood model 1 - log-likelihood model 2 = 0)
- Alt hypothesis: the log-likelihoods are not equal to one another (HA:
  log-likelihood model 1 - log-likelihood model 2 ≠ 0)

Interpretation: If the p value for the test is \< .05, then we reject
the null hypothesis and one model improves the fit of the model.

The likelihood ratio test compares the fit statistic from two nested
models. Nested models contain the same dependent variables, with one
having a subset of the other’s independent variables. The must be run on
the same sample.

NB. If the additional variable has many NAs, it may lead to too many
dropped rows, making the sample of the larger model smaller than the
smaller model. Drop the corresponding rows from the “smaller” model to
compensate.

## Drop observations with missing values

``` r
MAR <- MAR %>% 
  drop_na(sepkin, trsntlkin, kinmatsup, kinmilsup, grplang, pct_ctry)
```

## Run two or more nested models

``` r
fit_a <- glm(sepkin ~ trsntlkin,
             data = MAR, family = binomial(link = "logit"))
fit_b <- glm(sepkin ~ trsntlkin + grplang + pct_ctry,
             data = MAR, family = binomial(link = "logit"))
fit_c <- glm(sepkin ~ trsntlkin + kinmatsup + kinmilsup + grplang + pct_ctry,
             data = MAR, family = binomial(link = "logit"))
```

``` r
map(list(fit_a, fit_b, fit_c), summary)
```

    [[1]]

    Call:
    glm(formula = sepkin ~ trsntlkin, family = binomial(link = "logit"), 
        data = MAR)

    Coefficients:
                                                Estimate Std. Error z value
    (Intercept)                                  -2.8034     0.4204  -6.668
    trsntlkinClose Kindred/Not Adjoining Base     1.9319     0.4362   4.429
    trsntlkinClose Kindred, Adjoining Base        1.8906     0.4487   4.213
    trsntlkinClose Kindred, More than 1 Country   2.5649     0.4469   5.740
                                                Pr(>|z|)    
    (Intercept)                                 2.60e-11 ***
    trsntlkinClose Kindred/Not Adjoining Base   9.47e-06 ***
    trsntlkinClose Kindred, Adjoining Base      2.52e-05 ***
    trsntlkinClose Kindred, More than 1 Country 9.47e-09 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for binomial family taken to be 1)

        Null deviance: 1013.80  on 836  degrees of freedom
    Residual deviance:  959.09  on 833  degrees of freedom
    AIC: 967.09

    Number of Fisher Scoring iterations: 5


    [[2]]

    Call:
    glm(formula = sepkin ~ trsntlkin + grplang + pct_ctry, family = binomial(link = "logit"), 
        data = MAR)

    Coefficients:
                                                 Estimate Std. Error z value
    (Intercept)                                 -3.053297   0.480804  -6.350
    trsntlkinClose Kindred/Not Adjoining Base    1.854847   0.438831   4.227
    trsntlkinClose Kindred, Adjoining Base       1.763777   0.451477   3.907
    trsntlkinClose Kindred, More than 1 Country  2.491353   0.448909   5.550
    grplangSome Speak Native Lang                0.231829   0.235364   0.985
    grplangMost Speak Native Lang                1.030939   0.272657   3.781
    pct_ctry                                    -0.001023   0.006064  -0.169
                                                Pr(>|z|)    
    (Intercept)                                 2.15e-10 ***
    trsntlkinClose Kindred/Not Adjoining Base   2.37e-05 ***
    trsntlkinClose Kindred, Adjoining Base      9.36e-05 ***
    trsntlkinClose Kindred, More than 1 Country 2.86e-08 ***
    grplangSome Speak Native Lang               0.324634    
    grplangMost Speak Native Lang               0.000156 ***
    pct_ctry                                    0.865976    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for binomial family taken to be 1)

        Null deviance: 1013.80  on 836  degrees of freedom
    Residual deviance:  938.83  on 830  degrees of freedom
    AIC: 952.83

    Number of Fisher Scoring iterations: 5


    [[3]]

    Call:
    glm(formula = sepkin ~ trsntlkin + kinmatsup + kinmilsup + grplang + 
        pct_ctry, family = binomial(link = "logit"), data = MAR)

    Coefficients:
                                                  Estimate Std. Error z value
    (Intercept)                                 -3.0587629  0.4824304  -6.340
    trsntlkinClose Kindred/Not Adjoining Base    1.8558380  0.4394281   4.223
    trsntlkinClose Kindred, Adjoining Base       1.6448396  0.4544475   3.619
    trsntlkinClose Kindred, More than 1 Country  2.4308115  0.4495357   5.407
    kinmatsupMaterial kinship support           -0.2695913  0.3839679  -0.702
    kinmilsupMilitary kinship support            1.5463083  0.4640465   3.332
    grplangSome Speak Native Lang                0.2350165  0.2380001   0.987
    grplangMost Speak Native Lang                1.0265800  0.2753856   3.728
    pct_ctry                                    -0.0002825  0.0061351  -0.046
                                                Pr(>|z|)    
    (Intercept)                                 2.29e-10 ***
    trsntlkinClose Kindred/Not Adjoining Base   2.41e-05 ***
    trsntlkinClose Kindred, Adjoining Base      0.000295 ***
    trsntlkinClose Kindred, More than 1 Country 6.40e-08 ***
    kinmatsupMaterial kinship support           0.482605    
    kinmilsupMilitary kinship support           0.000862 ***
    grplangSome Speak Native Lang               0.323415    
    grplangMost Speak Native Lang               0.000193 ***
    pct_ctry                                    0.963279    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for binomial family taken to be 1)

        Null deviance: 1013.80  on 836  degrees of freedom
    Residual deviance:  926.81  on 828  degrees of freedom
    AIC: 944.81

    Number of Fisher Scoring iterations: 5

## Run the likelihood ratio test

``` r
lrtest(fit_b, fit_a)
```

    Likelihood ratio test

    Model 1: sepkin ~ trsntlkin + grplang + pct_ctry
    Model 2: sepkin ~ trsntlkin
      #Df  LogLik Df  Chisq Pr(>Chisq)    
    1   7 -469.41                         
    2   4 -479.54 -3 20.259    0.00015 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The log likelihood of model two is -469.41, while model one is -479.54,
with a p value less than 0.05, so model 2 is better.

``` r
lrtest(fit_c, fit_b)
```

    Likelihood ratio test

    Model 1: sepkin ~ trsntlkin + kinmatsup + kinmilsup + grplang + pct_ctry
    Model 2: sepkin ~ trsntlkin + grplang + pct_ctry
      #Df  LogLik Df  Chisq Pr(>Chisq)   
    1   9 -463.40                        
    2   7 -469.41 -2 12.021   0.002452 **
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Model 3 is better.

# AIC and BIC

Akaike/Bayesean Information Criteria. Both rely on log-likelihood
statistics and MLE. BIC penalizes complicated models with more
covariates. Lower is better.

Models do not need to be nested, but must run on the same sample.

``` r
fit_d <- glm(sepkin ~ kinmatsup + kinmilsup,
             data = MAR, family = binomial(link = "logit"))
fit_b$aic; fit_d$aic
```

    [1] 952.8286

    [1] 1004.345

Model 1 is better.

``` r
BIC(fit_b); BIC(fit_d)
```

    [1] 985.9374

    [1] 1018.535

Again, model 1 is better.
