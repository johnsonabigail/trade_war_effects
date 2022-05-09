out = lm(Y[X < 0 & X >= -20] ~ X[X < 0 & X >= -20])
> left_intercept = out$coefficients[1]
> print(left_intercept)
(Intercept)
12.62254
> out = lm(Y[X >= 0 & X <= 20] ~ X[X >= 0 & X <= 20])
> right_intercept = out$coefficients[1]
> print(right_intercept)
(Intercept)
15.54961
> difference = right_intercept - left_intercept
> print(paste("The RD estimator is", difference, sep = " "))

T_X = X * T
> out = lm(Y[X >= -20 & X <= 20] ~ X[X >= -20 & X <= 20] + T[X >=
                                                               + -20 & X <= 20] + T_X[X >= -20 & X <= 20])
> summary(out)
Call:
  lm(formula = Y[X >= -20 & X <= 20] ~ X[X >= -20 & X <= 20] +
       T[X >= -20 & X <= 20] + T_X[X >= -20 & X <= 20])
Residuals:
  Min 1Q Median 3Q Max
-17.373 -7.718 -0.755 6.384 33.697
Coefficients:
  Estimate Std. Error t value Pr(>|t|)
(Intercept) 12.62254 0.77459 16.296 < 2e-16 ***
  X[X >= -20 & X <= 20] -0.24807 0.06723 -3.690 0.000238 ***
  T[X >= -20 & X <= 20] 2.92708 1.23529 2.370 0.018024 *
  T_X[X >= -20 & X <= 20] 0.12612 0.12459 1.012 0.311667


w = NA
> w[X < 0 & X >= -20] = 1 - abs(X[X < 0 & X >= -20]/20)
> w[X >= 0 & X <= 20] = 1 - abs(X[X >= 0 & X <= 20]/20)


out = lm(Y[X < 0] ~ X[X < 0], weights = w[X < 0])
> left_intercept = out$coefficients[1]
> out = lm(Y[X >= 0] ~ X[X >= 0], weights = w[X >= 0])
> right_intercept = out$coefficients[1]
> difference = right_intercept - left_intercept
> print(paste("The RD estimator is", difference, sep = " "))

out = rdrobust(Y, X, kernel = "uniform", p = 1, h = 20)
> summary(out)

out = rdrobust(Y, X, kernel = "triangular", p = 1, h = 20)
> summary(out)

out = rdrobust(Y, X, kernel = "triangular", p = 2, h = 20)
> summary(out)

out = rdbwselect(Y, X, kernel = "triangular", p = 1, bwselect = "mserd")
> summary(out)

out = rdbwselect(Y, X, kernel = "triangular", p = 1, bwselect = "msetwo")
> summary(out)

out = rdrobust(Y, X, kernel = "triangular", p = 1, bwselect = "mserd")
> summary(out)

rdout = rdrobust(Y, X, kernel = "triangular", p = 1, bwselect = "mserd")
> print(names(rdout)[1:7])

bandwidth = rdrobust(Y, X, kernel = "triangular", p = 1, bwselect = "mserd")$bws[1,
                                                                                 + 1]
> out = rdplot(Y[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
               + p = 1, kernel = "triangular", cex.axis = 1.5, cex.lab = 1.5)
> summary(out)

out = rdrobust(Y, X, kernel = "triangular", scaleregul = 0, p = 1,
               + bwselect = "mserd")
> summary(out)


out = rdrobust(Y, X, kernel = "triangular", p = 1, bwselect = "mserd")
> summary(out)

out = rdrobust(Y, X, kernel = "triangular", p = 1, bwselect = "mserd",
               + all = TRUE)
> summary(out)



out = rdrobust(Y, X, kernel = "triangular", p = 1, bwselect = "cerrd")
> summary(out)


out = rdbwselect(Y, X, kernel = "triangular", p = 1, all = TRUE)
> summary(out)


Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          + data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
> colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994", "merkezi",
                  + "merkezp", "subbuyuk", "buyuk")
> out = rdbwselect(Y, X, covs = Z, kernel = "triangular", scaleregul = 1,
                   + p = 1, bwselect = "mserd")
> summary(out)


Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          + data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
> colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994", "merkezi",
                  + "merkezp", "subbuyuk", "buyuk")
> out = rdrobust(Y, X, covs = Z, kernel = "triangular", scaleregul = 1,
                 + p = 1, bwselect = "mserd")
> summary(out)


out = rdrobust(Y, X, kernel = "triangular", scaleregul = 1, p = 1,
               + bwselect = "mserd", cluster = data$prov_num)
> summary(out)


Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          + data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
> colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994", "merkezi",
                  + "merkezp", "subbuyuk", "buyuk")
> out = rdrobust(Y, X, covs = Z, kernel = "triangular", scaleregul = 1,
                 + p = 1, bwselect = "mserd", cluster = data$prov_num)
> summary(out)



