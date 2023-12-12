rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#compare it to the output from installed.packages()[,"Package"] and install the missing packages
list.of.packages <- c("fBasics","expm","uroot","tseries","mgcv","MASS","leaps","TSA","matrixcalc","mvtnorm","tsDyn", "vars","MTS","fpp3","svars","vars","fpp2","githubinstall","HI", "mvnfast","minqa","gridExtra","rlang","gridExtra" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#library(MTS) # needed for MTSplot,ccm,VARMAsim
#library(mvtnorm) #needed for rmvnorm 
#library(matrixcalc) #needed for matrix.power
lapply(list.of.packages, require, character.only = TRUE)

# 1. Data import and visualization ----



y.data <- ts(read.table("e11.txt",header=T),start=c(1960,1),
             end=c(1982,4), frequency=4)
y <- window(y.data,start=c(1960,1),end=c(1978,4))
dy <- diff(log(y), d = 1)


# 2. VAR model selection criteria ----

crit <- vars::VARselect(dy, lag.max = 4, type = "const")


crit$selection
round(crit$criteria, digits = 4)


VAR_aic <- vars::VAR(dy, lag.max = 4, type = "const", ic = "AIC")






summary(VAR_aic)





dy_tsbl <- fable::as_tsibble(dy, pivot_longer = FALSE) 
fit <- dy_tsbl %>% 
  model(VAR = fable::VAR(vars(invest, income, cons) ~ 1 + AR(p = 1:4), 
                         ic = "bic"))
fit %>% tidy() %>% knitr::kable(digits = 2)


# 3. Residual time plots ----

ts(resid(VAR_aic),start = c(1960,1), end = c(1978,4), frequency = 4) %>% forecast::autoplot()

## 3.1. Residual ACFs and CCFs ----
ts(resid(VAR_aic),start = c(1960,1), end = c(1978,4), frequency = 4) %>% forecast::ggAcf()

## 3.2. Multivariate Portmanteau tests ----
for (j in 7:10){
  show(vars::serial.test(VAR_aic, lags.pt = j, type = "PT.asymptotic"))  
}

for (j in 7:10){
  show(vars::serial.test(VAR_aic, lags.pt = j, type = "PT.adjusted"))  
}


## 3.3. Multivariate  Breusch-Godfrey LM test ----
for (j in 1:4){
  show(vars::serial.test(VAR_aic, lags.bg = j, type = "BG"))  
}


for (j in 1:4){
  show(vars::serial.test(VAR_aic, lags.bg = j, type = "ES"))  
}

## 3.4. Residuals normality ----
### 3.4.1 Multivariate Normality Test ----
vars::normality.test(VAR_aic, multivariate.only = TRUE)
### 3.4.2 Residual ARCH test ----
for (j in 1:4){
  show(vars::arch.test(VAR_aic, lags.multi = j, multivariate.only = TRUE))  
}

### 3.4.3 Test for structural change ----
plot(stability(VAR_aic, type = "OLS-CUSUM"))

