---
title: "RegMod_Motor Trend"
author: "Fatin Fatihah Zahari"
date: "December 27, 2015"
output: pdf_document
---


# 1.0 Executive Summary

For your car to be able to get from point A to point B without having to stay trudging along in first gear, it needs a working transmission. The transmission allows the vehicle to change gears, thereby transferring power from the engine to the drive axle in the most efficient way possible. It does this by varying the gear ratio. In lower gears, this increases available power while reducing speed. Higher gears, on the other hand, reduce power and increase speed. This enables cars to distribute power and speed in the most efficient way for any given situation. But whilst everyone agrees that a transmission is absolutely vital to the inner workings of any car, there is no general consensus regarding what kind of transmission is better-automatic or manual.This document shows outcome of the studies done. 

# 2.0 Data analysis

`mtcars` dataset was used for the analysis. It comprises 10 aspects of automobile design and performance for 32 automobiles.


```r
# factor some variables
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
str(mtcars)
```

```
## 'data.frame':	32 obs. of  11 variables:
##  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
##  $ cyl : Factor w/ 3 levels "4","6","8": 2 2 1 2 3 2 3 1 1 2 ...
##  $ disp: num  160 160 108 258 360 ...
##  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
##  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
##  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
##  $ qsec: num  16.5 17 18.6 19.4 17 ...
##  $ vs  : Factor w/ 2 levels "0","1": 1 1 2 2 1 2 1 2 2 2 ...
##  $ am  : Factor w/ 2 levels "0","1": 2 2 2 1 1 1 1 1 1 1 ...
##  $ gear: Factor w/ 3 levels "3","4","5": 2 2 2 1 1 1 1 2 2 2 ...
##  $ carb: Factor w/ 6 levels "1","2","3","4",..: 4 4 1 1 2 1 4 2 2 4 ...
```

The following boxplot shows the relation between the transmission type and the miles per gallon (MPG).


```r
library(plyr)
library(ggplot2)
# Rename the levels of transmission types
transmission <- revalue(mtcars$am, c('0'="automatic", '1'="manual"))
ggplot(mtcars, aes(x=transmission, y=mpg, fill=transmission)) +
    geom_boxplot() +
    xlab("Transmission type") +
    ylab("Miles per gallon")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

It suggests a clear difference on fuel consumption between automatic
and manual transmission cars. Below is the model to explain the MPG
variability with the transmission type _only_.


```r
fit1 <- lm(mpg ~ am, data=mtcars)
summary(fit1)
```

```
## 
## Call:
## lm(formula = mpg ~ am, data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.3923 -3.0923 -0.2974  3.2439  9.5077 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   17.147      1.125  15.247 1.13e-15 ***
## am1            7.245      1.764   4.106 0.000285 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.902 on 30 degrees of freedom
## Multiple R-squared:  0.3598,	Adjusted R-squared:  0.3385 
## F-statistic: 16.86 on 1 and 30 DF,  p-value: 0.000285
```

Although coefficients for both intercept and the transmission type are
significant, the model fit using _only_ transmission type explains only
35.9798943% of the MPG variation.



Before making any conclusions on the effect of transmission type on fuel
efficiency, we look at the variances between several variables in the dataset.


```r
pairs(mtcars, panel=function(x,y) {
    points(x, y)
    abline(lm(y ~ x), col="red")
})
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Based on the pairs plot above, several variables seem to have high
correlation with the `mpg` variable. Hence, we build an initial model
using all variables and select the model with the best subset of
predictors using stepwise backward elimination and forward selection.


```r
initial_model <- lm(mpg ~ ., data=mtcars)
best_model <- step(initial_model, direction="both", trace=0)
summary(best_model)
```

```
## 
## Call:
## lm(formula = mpg ~ cyl + hp + wt + am, data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.9387 -1.2560 -0.4013  1.1253  5.0513 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 33.70832    2.60489  12.940 7.73e-13 ***
## cyl6        -3.03134    1.40728  -2.154  0.04068 *  
## cyl8        -2.16368    2.28425  -0.947  0.35225    
## hp          -0.03211    0.01369  -2.345  0.02693 *  
## wt          -2.49683    0.88559  -2.819  0.00908 ** 
## am1          1.80921    1.39630   1.296  0.20646    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.41 on 26 degrees of freedom
## Multiple R-squared:  0.8659,	Adjusted R-squared:  0.8401 
## F-statistic: 33.57 on 5 and 26 DF,  p-value: 1.506e-10
```

```r
par(mfrow = c(2,2))
plot(best_model)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

The final model contains four predictors, `cyl` (number of cylinders),
`hp` (horsepower), `weight` (weight) and `am` (transmission type). This
model explains the 86.5879872% of the MPG
variation. The number of cylinders, weight and horsepower  significantly
contribute to the accuracy of the model while the transmission has no effect
on the fuel consumption ($\alpha=0.05$). Also the residual plots show that
the distribution of residuals seem to be
normally distributed and not depending on fitted values.

# 3.0 Results

The data analysis on `mtcars` dataset from 1973 reveals some interesting
points.

- If a car has 6 cylinder or 8 cylinder, rather than 4, the fuel consumption
increases by 3.0313445 and
2.1636753 MPG, respectively.
- One unit of increase on gross horsepower results
0.0321094 less MPG, an increase on the fuel
consumption.
- 1000 lb increase on the weight of a car, everything else same, yields
2.4968294 less MPG, again an increase on the
fuel consumption.

The `mtcars` dataset used for this analysis comprises data for 1973-1974
models. This analysis was not able to find any significant link between the
transmission type and fuel consumption. For modern cars, with much more
efficient automatic transmission system, it is less likely that having
a stick shift car will save you any money.
