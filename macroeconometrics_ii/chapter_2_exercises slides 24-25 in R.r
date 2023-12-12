rm(list = ls())

install.packages("MTS")
install.packages("rmvnorm")
install.packages("matrixcalc")
install.packages("TSA")



library(MTS) # needed for MTSplot,ccm,VARMAsim
library(mvtnorm) #needed for rmvnorm 
library(matrixcalc) #needed for matrix.power
library(TSA)
library(leaps)
library(MASS)
library(mgcv)
library(tseries)
library(uroot)
##############################DGP1 - three variables VAR(1) #####################

nu = c(1,0.5,2)
A1 = matrix(c(0.5,0.1,0,0,0.1,0.2,0,0.3,0.3),nrow=3,ncol=3)
sigma.u=matrix(c(2.25,rep(0,3),1,0.5,0,0.5,0.74),nrow=3,ncol=3)

#(1)
eig=eigen(A1)
eig$values
abs(eig$values)

#(2)
mu.y=solve(diag(3)-A1)%*%nu
mu.y

#(3)
A=kronecker(A1,A1)
gamma.0=solve(diag(9)-A)%*%vec(sigma.u)
gam.0=matrix(gamma.0,nrow=3,ncol=3)
gam.1=A1%*%gam.0
gam.2=A1%*%gam.1
gam.3=A1%*%gam.2
gam.3_=matrix.power(A1,3)%*%gam.0
D=diag(sqrt(diag(gam.0)))
Di=solve(D)
R.0=Di%*%gam.0%*%Di
R.1=Di%*%gam.1%*%Di
R.2=Di%*%gam.2%*%Di
R.3=Di%*%gam.3%*%Di


gam.0
gam.1
gam.2
gam.3
R.0
R.1
R.2
R.3

#(4)
Phi.1=A1
Phi.2=matrix.power(A1,2)
Phi.3=matrix.power(A1,3)


Phi.10=matrix.power(A1,10)
Phi.100=matrix.power(A1,100)


Phi.1
Phi.2
Phi.3

#(5)
set.seed(123456)
y=VARMAsim(nobs=1000,cnst=nu,arlags=1,phi=A1,sigma=sigma.u)

par(mar = c(1, 1, 1, 1))


MTSplot(y$noises)
MTSplot(y$series)
mu.y

mu.y
mean(y$series[,1])
mean(y$series[,2])
mean(y$series[,3])




R.0
R.1
R.2
R.3
ccm(y$series,lag=3,level=T)


##############################DGP2 - Bivariate VAR(2) #####################
nu = c(3,4)
A1 = matrix(c(0.5,0.4,0.1,0.5),nrow=2,ncol=2)
A2 = matrix(c(0,0.25,0,0),nrow=2,ncol=2)
sigma.u = matrix(c(0.09,0,0,0.04),nrow=2,ncol=2)

#(1)
zeros=matrix(c(0,0,0,0),nrow=2,ncol=2)

A=matrix(rbind(cbind(A1,A2),cbind(diag(2),zeros)),nrow=4,ncol=4)
sigma.u1=matrix(rbind(cbind(sigma.u,zeros),cbind(zeros,zeros)),nrow=4,ncol=4)

eig=eigen(A)
eig$values
abs(eig$values)


#(2)

nu1=c(nu,0,0)
mu.y=solve(diag(4)-A)%*%nu1
mu.y1=solve(diag(2)-A1-A2)%*%nu


#(3)

Ak=kronecker(A,A)
gamma.0=solve(diag(16)-Ak)%*%vec(sigma.u1)
gam.0=matrix(gamma.0,nrow=4,ncol=4)
gam.1=A%*%gam.0
gam.2=A%*%gam.1
gam.3=A%*%gam.2
gam.3_=matrix.power(A,3)%*%gam.0


D=diag(sqrt(diag(gam.0)))
Di=solve(D)
R.0=Di%*%gam.0%*%Di
R.1=Di%*%gam.1%*%Di
R.2=Di%*%gam.2%*%Di
R.3=Di%*%gam.3%*%Di

#(4)
Phi.1_=A1
Phi.2_=Phi.1_%*%A1+A2
Phi.3_=Phi.2_%*%A1+Phi.1_%*%A2
Phi.1=A
Phi.2=matrix.power(A,2)
Phi.3=matrix.power(A,3)

#(5)

set.seed(123456)
y=VARMAsim(nobs=300,cnst=nu,arlags=2,phi=cbind(A1,A2),sigma=sigma.u)


par(mar = c(1, 1, 1, 1))


MTSplot(y$noises)
MTSplot(y$series)
mu.y

mu.y
mean(y$series[,1])
mean(y$series[,2])
par(mar = c(1, 1, 1, 1))

ccm(y$series,lag=3,level=T)

