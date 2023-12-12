rm(list = ls())

#Set Working Directory to the path that contains the opened R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#compare it to the output from installed.packages()[,"Package"] and install the missing packages
list.of.packages <- c("tsDyn", "vars","MTS","fpp3","svars","vars","fpp2","githubinstall","HI", "mvnfast","minqa","gridExtra","rlang","gridExtra" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#!!!!!!!!!!!!!!!!!!!
#install.packages(".../VARsignR_0.1.3.tar.gz", repos = NULL, type = "source")
#library(VARsignR)



#library(tsDyn)
# Nonlinear time series models with regime switching 
# but it also includes (linear and non linear VARs)

#library(vars)
# Contains several functions to fit and analyze VAR 
# and VEC models

#library(MTS)
# Useful for analyzing multivariate linear time series 
# and estimating multivariate volatility models



# SVAR identification with theory-based sign restrictions
#library(VARsignR)
#library(githubinstall)
#githubinstall("VARsignR")




#library(fpp3)
# Several functions for time series manipulation, 
# graphics and statistics, plotting,  fitting 
# several time series models and forecasting


#library(svars)
# it includes a rich variety of analysis tools 
# that are well known in the SVAR literature.

# 1. Data import and visualization ----


y.data <- ts(read.table("e11.txt",header=T),start=c(1960,1),end=c(1982,4), frequency=4)
y <- window(y.data,start=c(1960,1),end=c(1978,4))
autoplot(y)+labs(x = "Time", y = "values", title = "Time plot of the series in levels")



## 1.1. Plot of data in first differences ----


dy <- diff(log(y), d = 1)
plots <- list()
for (i in 1:3){
  plots[[i]] <- autoplot(dy[,i], color = "blue")+labs(x = "Time", y = colnames(y)[i])
}
gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], ncol = 2, nrow = 2)






# 2. Estimation of VAR processes ----

# 

## 2.1 VAR estimation results ----
### 2.1.1. using `vars` package ----

VAR_2 <- vars::VAR(dy, p = 2, type = "const")
summary(VAR_2)

## returns the  A1 and A2 matrices of the estimated VAR(2)
vars::Acoef(VAR_2)

## returns a matrix with all the parameters stacked - similar to B in slide 4 , Chapter 3
vars::Bcoef(VAR_2)


# returns Estimate  Std. Error     t value     Pr(>|t|) for all the parameters of the model
coef(VAR_2)

### 2.1.2. using `fpp3` package ----

# Estimation of VAR processes with `fpp3` 
dy_tsbl <- fable::as_tsibble(dy, pivot_longer = FALSE) 
fit <- dy_tsbl %>% model(VAR_1 = fable::VAR(vars(invest, income, cons) ~ AR(1)),
                         VAR_2 = fable::VAR(vars(invest, income, cons) ~ AR(2))) 
fit %>% tidy()

fit %>% glance()
fit %>% augment()







## 2.2 Check the stability of the fitted VAR ----
# using `vars` package
#Returns a vector of the eigenvalues of the companion coefficient matrix.
vars::roots(VAR_2) %>% 
  knitr::kable(digits = 2, col.names = "Inverse roots", 
               caption = "Inverse roots of the VAR polynomial")


## 2.3 Extracting VMA weights ----

Phi(VAR_2,nstep = 2)

## 2.4 Estimating VARs with restrictions ----
# using `vars` package
#?vars::restrict
VAR_2_r <- vars::restrict(VAR_2, method = "ser",thresh = 1.5)
vars::Bcoef(VAR_2_r)


VAR_1 <- vars::VAR(dy, p = 1, type = "const")
restrictions <- matrix(c(1, 0, 0, 1,
                         1, 1, 1, 1,
                         0, 1, 1, 1),
                       nrow=3, ncol=4, byrow=TRUE)
VAR_1_r <- vars::restrict(VAR_1, method = "man", 
                          resmat = restrictions)
vars::Bcoef(VAR_1_r)

## 2.5 Forecasting with fitted VAR models ----
### 2.5.1. using `vars` package ----

#  
VAR_2_prd <- predict(VAR_2, n.ahead = 4, ci = 0.95)
VAR_2_prd

par(mar = c(1, 1, 1, 1))

plot(VAR_2_prd)
fanchart(VAR_2_prd)

### 2.5.2. using `fpp3` package ----
fit %>% forecast(h=4)

dy_tsbl %>% 
  model(VAR_1 = fable::VAR(vars(invest, income, cons) ~ AR(1)),
        VAR_2 = fable::VAR(vars(invest, income, cons) ~ AR(2))) %>% 
  forecast(h = 4)
  dy_tsbl %>% 
  model(VAR_1 = fable::VAR(vars(invest, income, cons) ~ AR(1)),
        VAR_2 = fable::VAR(vars(invest, income, cons) ~ AR(2))) %>% 
  forecast(h = 4) %>% 
  autoplot(filter(dy_tsbl,year(index)>1975), level = NULL)
  
  ## 2.6 Granger and Instantaneous Causality ----
  
# `vars`: 
  vars::causality(VAR_2, cause = c("invest","income"))
  vars::causality(VAR_2, cause = c("invest","income"), vcov.=vcovHC(VAR_2))
  
  ## 2.6 Forecast error IRFs ----
  # `vars`: Forecast error IRFs
  
  ### 2.6.1. Non-orthogonalized ----
  
  plot(vars::irf(VAR_2, impulse = "income", response = "cons", 
                 n.ahead = 8, ortho = FALSE, cumulative = FALSE, 
                 boot = TRUE, ci = 0.95)) 
  plot(vars::irf(VAR_2, impulse = "income", response = "cons", 
                 n.ahead = 8, ortho = FALSE, cumulative = FALSE, 
                 boot = TRUE, ci = 0.975)) 
  plot(vars::irf(VAR_2, impulse = "cons", response = "invest", 
                 n.ahead = 8, ortho = FALSE, cumulative = FALSE, 
                 boot = TRUE, ci = 0.95)) 
  ### 2.6.2. Orthogonalized ----
  
  #orthogonalized
  plot(vars::irf(VAR_2, impulse = "cons", response = "invest", 
                 n.ahead = 8, ortho = TRUE, cumulative = FALSE, 
                 boot = TRUE, ci = 0.95)) 
  plot(vars::irf(VAR_2, impulse = "income", response = "invest", 
                 n.ahead = 8, ortho = TRUE, cumulative = FALSE, 
                 boot = TRUE, ci = 0.95))
  plot(vars::irf(VAR_2, impulse = "cons", response = "income", 
                 n.ahead = 8, ortho = TRUE, cumulative = FALSE, 
                 boot = TRUE, ci = 0.95)) 
  
  ## 2.7 Forecast Error Variance Decomposition  ----
  
# `vars`: Forecast Error Variance Decomposition 
  vars::fevd(VAR_2,n.ahead = 4)
  plot(vars::fevd(VAR_2,n.ahead = 4))
  