# Multinomial Logistic Regression


[Source](https://www.ariclabarr.com/logistic-regression/part_5_assess.html)

``` r
library(tidyverse)
library(vcdExtra)
library(VGAM)
```

``` r
gator <- Alligator
head(gator)
```

         lake  sex  size    food count
    1 Hancock male small    fish     7
    2 Hancock male small  invert     1
    3 Hancock male small reptile     0
    4 Hancock male small    bird     0
    5 Hancock male small   other     5
    6 Hancock male large    fish     4

# Generalized Logit Model

Uses relative risk ratios rather than odds ratios.

``` r
gator$food <- factor(gator$food)

gator <- gator %>% 
  filter(count > 0)

glogit_model <- vglm(
  food ~ size + lake + sex,
  data = gator,
  family = multinomial(refLevel = "fish"),
  weights = count
)

summary(glogit_model)
```


    Call:
    vglm(formula = food ~ size + lake + sex, family = multinomial(refLevel = "fish"), 
        data = gator, weights = count)

    Coefficients: 
                   Estimate Std. Error z value Pr(>|z|)   
    (Intercept):1  -1.70187    0.76907  -2.213  0.02690 * 
    (Intercept):2  -1.16724    0.53375  -2.187  0.02875 * 
    (Intercept):3  -1.72131    0.63138  -2.726  0.00641 **
    (Intercept):4  -2.85901    1.14564      NA       NA   
    sizesmall:1    -0.73024    0.65228  -1.120  0.26292   
    sizesmall:2     1.33626    0.41119   3.250  0.00116 **
    sizesmall:3     0.29058    0.45993   0.632  0.52752   
    sizesmall:4    -0.55704    0.64661  -0.861  0.38898   
    lakeHancock:1   0.57527    0.79522   0.723  0.46943   
    lakeHancock:2  -1.78051    0.62321  -2.857  0.00428 **
    lakeHancock:3   0.76658    0.56855   1.348  0.17756   
    lakeHancock:4   1.12946    1.19280   0.947  0.34369   
    lakeOklawaha:1 -0.55035    1.20987  -0.455  0.64919   
    lakeOklawaha:2  0.91318    0.47612   1.918  0.05511 . 
    lakeOklawaha:3  0.02606    0.77777   0.034  0.97327   
    lakeOklawaha:4  2.53026    1.12212   2.255  0.02414 * 
    lakeTrafford:1  1.23699    0.86610   1.428  0.15323   
    lakeTrafford:2  1.15582    0.49279   2.345  0.01900 * 
    lakeTrafford:3  1.55776    0.62567   2.490  0.01278 * 
    lakeTrafford:4  3.06105    1.12973   2.710  0.00674 **
    sexmale:1      -0.60643    0.68885  -0.880  0.37867   
    sexmale:2      -0.46296    0.39552  -1.171  0.24180   
    sexmale:3      -0.25257    0.46635  -0.542  0.58810   
    sexmale:4      -0.62756    0.68528  -0.916  0.35978   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Names of linear predictors: log(mu[,1]/mu[,2]), log(mu[,3]/mu[,2]), 
    log(mu[,4]/mu[,2]), log(mu[,5]/mu[,2])

    Residual deviance: 537.8655 on 200 degrees of freedom

    Log-likelihood: -268.9327 on 200 degrees of freedom

    Number of Fisher scoring iterations: 6 

    Warning: Hauck-Donner effect detected in the following estimate(s):
    '(Intercept):4'


    Reference group is level  2  of the response

# Interpretation

To get relative risk ratios.

``` r
100*(exp(coef(glogit_model)) - 1)
```

     (Intercept):1  (Intercept):2  (Intercept):3  (Intercept):4    sizesmall:1 
        -81.765832     -68.877434     -82.116909     -94.267429     -51.820636 
       sizesmall:2    sizesmall:3    sizesmall:4  lakeHancock:1  lakeHancock:2 
        280.479092      33.720655     -42.709537      77.760394     -83.144823 
     lakeHancock:3  lakeHancock:4 lakeOklawaha:1 lakeOklawaha:2 lakeOklawaha:3 
        115.238205     209.399475     -42.325256     149.223994       2.640013 
    lakeOklawaha:4 lakeTrafford:1 lakeTrafford:2 lakeTrafford:3 lakeTrafford:4 
       1155.676642     244.522908     217.663308     374.818559    2034.995379 
         sexmale:1      sexmale:2      sexmale:3      sexmale:4 
        -45.470516     -37.058401     -22.319776     -46.610637 

# Predictions and Diagnostics

Predicted Proababilities:

``` r
pred_probs <- predict(glogit_model, newdata = gator,
                      type = "response")
head(pred_probs, n = 10)
```

             bird      fish     invert     other    reptile
    1  0.05114890 0.6006519 0.07545711 0.2401562 0.03258584
    2  0.05114890 0.6006519 0.07545711 0.2401562 0.03258584
    3  0.05114890 0.6006519 0.07545711 0.2401562 0.03258584
    4  0.11022858 0.6236514 0.02059152 0.1864723 0.05905622
    5  0.11022858 0.6236514 0.02059152 0.1864723 0.05905622
    6  0.11022858 0.6236514 0.02059152 0.1864723 0.05905622
    7  0.07918783 0.5070799 0.10120825 0.2609978 0.05152616
    8  0.07918783 0.5070799 0.10120825 0.2609978 0.05152616
    9  0.07918783 0.5070799 0.10120825 0.2609978 0.05152616
    10 0.07918783 0.5070799 0.10120825 0.2609978 0.05152616
