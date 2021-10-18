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

# polinom regression

model2 = lm(y\~poly(x,2,raw=TRUE)) \# 2nd order print(model2)

model3 = lm(y\~poly(x,3,raw=TRUE)) \# 3nd order print(model3)

# itu jika kita selesaikan dengan base nya si R

# sekarang kita akan coba buat sendiri algoritmanya

# bagaimana caranya?

# function polinom

poli\_fitting\_ikanx = function(X, \# vector x Y, \# vector y M){ \#
order tertinggi si polinom

\# Input: \# X vektor absis 1 x n \# Y vektor ordinat 1 x n \# M derajat
polinom least square \# Output: C koefisien polinom least square

\# STEP 1 \# initial n = length(X) B = matrix(0,ncol = 1,nrow = (M+1)) F
= matrix(0,ncol = n,nrow = (M+1))

\# STEP 2 \# mengisi kolom-kolom F dengan pangkat-pangkat dari X for k =
1:M+1 F(:,k)=X’.^(k-1); end % Menyelesaikaq

\# dummy output return(list(n,B,F)) }

poli\_fitting\_ikanx(x,y,4)
