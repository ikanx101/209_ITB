Belajar Curve Fitting
================

# CURVE FITTING

Kita akan belajar dulu dari dasarnya. Kita juga akan lihat dan
bandingkan cara pengerjaan dengan algoritma bikinan sendiri dan
algoritma dar `base` **R**.

## Linear *Curve Fitting*

Sering disebut sebagai regresi linear.

``` r
# ini data contoh
x = c(32,64,96,118,126,144,152.5,158)
y = c(99.5,104.8,108.5,100,86,64,35.3,15)

# we will make y the response variable and x the predictor
# the response variable is usually on the y-axis
plot(x,y,pch=19)
```

![](cfit_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Untuk membuat model linear, kita bisa gunakan `base` **R** via `lm()`.
Berikut adalah caranya:

``` r
# linear regression
model1 = lm(y~x)
model1
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Coefficients:
    ## (Intercept)            x  
    ##    143.0438      -0.5966

``` r
n = length(x)
sum_xi = sum(x)
sum_xi_yi = sum(x*y)
sum_yi = sum(y)

A = matrix(c(n,sum_xi,sum_xi,sum_xi_yi),nrow = 2,byrow = T)
A
```

    ##       [,1]     [,2]
    ## [1,]   8.0   890.50
    ## [2,] 890.5 59912.45

``` r
A_inv = solve(A)
A_inv
```

    ##              [,1]          [,2]
    ## [1,] -0.190992145  2.838784e-03
    ## [2,]  0.002838784 -2.550283e-05

``` r
b = c(sum_yi,sum_xi_yi)

A_inv %*% b
```

    ##            [,1]
    ## [1,] 52.9812209
    ## [2,]  0.2125213
