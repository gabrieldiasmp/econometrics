
rm(list = ls()) 

#Set Working Directory to the path that contains the opened R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#compare it to the output from installed.packages()[,"Package"] and install the missing packages
list.of.packages <- c("ggplot2","readxl","dynlm","forecast","urca","vars" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


library(urca)  # ca.jo, ur.df, finland
library(vars)  # vec2var
library(tsDyn) # VECM


# level data : 1958q1 - 1984q2
data(finland)
lev <- finland
nr_lev <- nrow(lev)

yq <- expand.grid(1:4, 1958:1984)[1:nr_lev,]
colnames(yq) <- c("q", "yyyy")
rownames(yq) <- NULL


test1<-c(rep(0,20))
for (j in 1:length(test1)){
  aa<-fUnitRoots::adfTest(lev[,1],lags=j,type="c")
  test1[j]<-aa@test$statistic
}


test2<-c(rep(0,20))
for (j in 1:length(test2)){
  aa<-fUnitRoots::adfTest(lev[,2],lags=j,type="c")
  test2[j]<-aa@test$statistic
}



test3<-c(rep(0,20))
for (j in 1:length(test3)){
  aa<-fUnitRoots::adfTest(lev[,3],lags=j,type="c")
  test3[j]<-aa@test$statistic
}




test4<-c(rep(0,20))
for (j in 1:length(test4)){
  aa<-fUnitRoots::adfTest(lev[,4],lags=j,type="c")
  test4[j]<-aa@test$statistic
}









# quarterly centered dummy variables
yq$Q1 <- (yq$q==1)-1/4
yq$Q2 <- (yq$q==2)-1/4
yq$Q3 <- (yq$q==3)-1/4
dum_season <- yq[,-c(1,2)]

lev=ts(lev)



colnames(lev)=c("money","real income","interest rate","infl")

  plots <- list()
for (i in 1:4){
  plots[[i]] <- autoplot(lev[,i], color = "blue")+labs(x = "Time", y = colnames(lev)[i])
}
gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol = 2, nrow = 2)

# 1st differenced data
dif <- as.data.frame(diff(as.matrix(lev), lag = 1))

#### Cointegration Test#####
coint_ca.jo <- ca.jo(
  lev, ecdet = "none", type  = "eigen", K = 2, 
  spec = "transitory", season = 4, dumvar = NULL)
summary(coint_ca.jo)

#r=0 maximum eigenvalue
#-106*log(1-0.30932660)
#r=1 maximum eigenvalue
#-106*log(1-0.22599561)


#########Trace test
coint_ca.jo <- ca.jo(
  lev, ecdet = "none", type  = "trace", K = 2, 
  spec = "transitory", season = 4, dumvar = NULL)
summary(coint_ca.jo)

#r=0
#-106*log(1-0.30932660)-106*log(1-0.22599561)-106*log(1-0.07308056)-106*log(1-0.02946699)




### VECM estimation####
cajorls_ca.jo <- cajorls(coint_ca.jo, r=2)

vec2var_ca.jo <- vec2var(coint_ca.jo, r=2)

