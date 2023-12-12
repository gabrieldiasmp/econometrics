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




# VAR(1)  with 3 variables: K=3 and p=1 ----

#STABLE VECTOR AUTOREGRESSIVE PROCESSES - Chapter 2 Lütkepohl
# VAR model
#we have to define the VAR order p and the number of variables K
#Let us assume that K=3 and p=1

## 1.Define the parameters and var/cov of errors matrices  ----


#1.Define the matrix of parameters A1 
A1 <- c(0.5,0,0,0.1,0.1,0.3,0,0.2,0.3)
A1 <- matrix(A1, nrow = 3, ncol = 3,byrow=TRUE)


#2.Define the vector of constants v
v=c(1,0.5,2)
v <- matrix(v, nrow = 3, ncol =1,byrow=TRUE)

#3.Define the variance covariance matrix of u
sigma_u=c(2.25,0,0,0,1,0.5,0,0.5,0.74)
sigma_u<- matrix(sigma_u, nrow = 3, ncol = 3,byrow=TRUE)


## 2.Stationarity and Stability Conditions ----

### 2.1. See if VAR is sationary -  Proposition Slide 4 Ch2----
#VAR(1) is stationary and stable the roots of |I_K-A1z| are outside the unir circle
#or if the eigenvalues of matrix A1 are inside the unit circle
#we will use the eigenvalues of A1
eig=eigen(A1)
eig$values
abs(eig$values)<1


### 2.2. Expected value----
#(I_K-A1)^-1*v
mu.y=solve(diag(3)-A1)%*%v    ##solve function computes the inverse of a matrix
mu.y

### 2.3. Autocovariance functions----
#vec(Gamma_y(0)) ----slide 11 
A=kronecker(A1,A1)
gam.0=solve(diag(9)-A)%*%vec(sigma_u)
gam.0=matrix(gam.0,nrow=3,ncol=3)
#Gamma(h)=A1^h * Gamma(0)
#or Gamma(h)=A1 * Gamma(h-1)

gam.1=A1%*%gam.0
gam.2=A1%*%gam.1
gam.3=A1%*%gam.2

### 2.4. Autocorrelation functions----

#D^-1*A1^h*Gamma(0)*D^-1
#slide 11: D is the square root of the diagonal of Gamma(0),variance covariance matrix of y

D=diag(sqrt(diag(gam.0)))
Di=solve(D)
R.0=Di%*%gam.0%*%Di
R.1=Di%*%gam.1%*%Di
R.2=Di%*%gam.2%*%Di
R.3=Di%*%gam.3%*%Di


### 2.5. VMA representation----
#use the general formula in Slide 21 to get Phi_i from A1 
Phi.1=A1
#since Phi_0=I_K
#note that A_i=0 for i>p. in this case, for i>1, since the process is VAR(1)
Phi.2=Phi.1%*%A1
#OR
#Phi.2=matrix.power(A1,2)
Phi.3=Phi.2%*%A1
#OR
#Phi.3=matrix.power(A1,3)

## 3. Forecasting ----
#see slide 28- the optimal forecast is the expected value  conditional on the information available at time T 
#see slide 42- general forma to optain forecasts h-steps ahead-

### 3.1. Point forecast----

#vector y_T
y_T=c(-6,3,5)
y_T <- matrix(y_T, nrow = 3, ncol =1,byrow=TRUE)

#1-step ahead optimal forecast y_{T+1}
y_h1=v+A1%*%y_T
#2-step ahead optimal forecast y_{T+2}
y_h2=v+A1%*%y_h1
### 3.2. MSE matrices----
# see general formula in slide 44
#we need Phi_i computed in 2.5 (VMA representation) and sigma_u defined in line 48
MSE_h1=sigma_u
MSE_h2=sigma_u+Phi.1%*%sigma_u%*%t(Phi.1)

### 3.3. Forecast intervals----
#y_t=(y_1t,y_2t,y_3t)

#see slide 37 to construct the CI
#confidence interval for y1t at 95%
#CI.lb - confidence interval lower bound
CI.lb_y1_h1=y_h1[1]-1.96*(MSE_h1[1,1])^0.5
#CI.ub - confidence interval upper bound
CI.ub_y1_h1=y_h1[1]+1.96*(MSE_h1[1,1])^0.5
#h=2
CI.lb_y1_h2=y_h2[1]-1.96*(MSE_h2[1,1])^0.5
CI.ub_y1_h2=y_h2[1]+1.96*(MSE_h2[1,1])^0.5

#confidence interval for y2t at 95%
#CI.lb - confidence interval lower bound
CI.lb_y2_h1=y_h1[2]-1.96*(MSE_h1[2,2])^0.5
#CI.ub - confidence interval upper bound
CI.ub_y2_h1=y_h1[2]+1.96*(MSE_h1[2,2])^0.5
#h=2
CI.lb_y2_h2=y_h2[2]-1.96*(MSE_h2[2,2])^0.5
CI.ub_y2_h2=y_h2[2]+1.96*(MSE_h2[2,2])^0.5

#confidence interval for y3t at 95%
#CI.lb - confidence interval lower bound
CI.lb_y3_h1=y_h1[3]-1.96*(MSE_h1[3,3])^0.5
#CI.ub - confidence interval upper bound
CI.ub_y3_h1=y_h1[3]+1.96*(MSE_h1[3,3])^0.5
#h=2
CI.lb_y3_h2=y_h2[3]-1.96*(MSE_h2[3,3])^0.5
CI.ub_y2_h2=y_h2[3]+1.96*(MSE_h2[3,3])^0.5

### 3.4. Forecast limit----
#see slide 33
# as h tends to infinity the forecasts tend to the expected value of y
lim_y_T_h_inf=solve(diag(3)-A1)%*%v

### 3.5. MSE matrices limit----
# slide 35- for h large MSE of the forecast tends to the covariance matrix of y gam.0 in line 71
lim_MSE_h_inf=solve(diag(9)-A)%*%vec(sigma_u)
lim_MSE_h_inf=matrix(lim_MSE_h_inf,nrow=3,ncol=3)

## 4. Causality ----
### 4.1. Granger Causality----
#slide 57
# in order to check whether Granger causality we have to check relevant restrictions on the A1 matrix 
#y_t=(y_1t,y_2t,y_3t)


if (A1[1,2]==0) {
  print("y_2t does not Granger cause y_1t ")
  
} else {
  print("y_2t  Granger causes y_1t  ")
}



if (A1[1,3]==0) {
  print("y_3t does not Granger cause y_1t ")
  
} else {
  print("y_3t  Granger causes y_1t  ")
}


if (A1[2,3]==0) {
  print("y_3t does not Granger cause y_2t ")
  
} else {
  print("y_3t  Granger causes y_2t  ")
}







### 4.2. Multi-step Causality----
# we saw above that information in y_2t cannot be used to improve the 1-step ahead forecast of y_1T
#however, it is still possible that it can be used to improve the h-step forecasts, h=2,...
#multi-step casuality
#a concept which refer explicitly to the forecast horizon

# 1-step casuality



if (A1[1,2]==0) {
  print("y_2t is not 1-step ahead causal for y_1t ")
  
} else {
  print("y_2t is 1-step ahead causal for y_1t ")
}



if (A1[1,3]==0) {
  print("y_3t is not 1-step ahead causal for y_1t ")
  
} else {
  print("y_3t is 1-step ahead causal for y_1t ")
}

if (A1[3,1]==0) {
  print("y_1t is not 1-step ahead causal for y_3t ")
  
} else {
  print("y_1t is 1-step ahead causal for y_3t ")
}





# 2-step casuality
A1_2=A1%*%A1


if (A1_2[1,2]==0) {
  print("y_2t is not 2-step ahead causal for y_1t ")
  
} else {
  print("y_2t is 2-step ahead causal for y_1t ")
}



if (A1_2[1,3]==0) {
  print("y_3t is not 2-step ahead causal for y_1t ")
  
} else {
  print("y_3t is 2-step ahead causal for y_1t ")
}

if (A1_2[3,1]==0) {
  print("y_1t is not 2-step ahead causal for y_3t ")
  
} else {
  print("y_1t is 2-step ahead causal for y_3t ")
}




#infinite step noncasuality requires that the relevant elements of A1 and A1^2
#for a VAR(1) process we only have to check A1 and A1^2



if (A1[1,2]==0&A1_2[1,2]==0) {
  print("y_2t is not infinite-step ahead causal for y_1t ")
  
} else {
  print("y_2t is infinite-step ahead causal for y_1t ")
}




if (A1[1,3]==0&A1_2[1,3]==0) {
  print("y_3t is not infinite-step ahead causal for y_1t ")
  
} else {
  print("y_3t is infinite-step ahead causal for y_1t ")
}




if (A1[3,1]==0&A1_2[3,1]==0) {
  print("y_1t is not infinite-step ahead causal for y_3t ")
  
} else {
  print("y_1t is infinite-step ahead causal for y_3t even if the same does not happen for the 1-step ahead is noncasual")
}

#if one of the conditions is false, y_1T if  infinite step causal for y3_T
# since now the infinite-step causality corresponds to the origibal concept
#of Granger casuality, y_1t Granger causes y_3T 





## 5. Impulse Response Analysis----
#Slide 64
#The impulse response function can be obtained from the VMA coefficient matrices derived from the VAR. In particular,
#element (j, k) of matrix ??i represents the response of variable j
#to a unit shock in variable k, after i periods.

#3x3 ERROR VARIANCE COVARIANCE MATRIX

##chol() function finds the upper triangular matrix P such that P'P=sigma_u. we need the lower triangular matrix P'; t() transpose


P1 <- t(chol(sigma_u)) 

## 5.1 Not orthogonalized----

#we only need the Phi_i matrices
Phi.0=diag(3)
Phi.0[3,2]
Phi.1[3,2]
Phi.2[3,2]
Phi.3[3,2]

##this element of Phi.0, Phi.1, Phi.2 and Phi.3 give us the effect of y2t on y3t zero, one, two and three periods after the shock


## 5.2 Orthogonalized----

#We have to obtain the P matrix first - slide 72
P2 <- t(chol(sigma_u))

#Orthogonal Impulse responses matrices
theta_0=P2
theta_1=Phi.1%*%P2
theta_2=Phi.2%*%P2
theta_3=Phi.3%*%P2

#for instance, element [1,1] from theta_0,theta_1,theta_2,theta_3 gives us, respectively, the effect of epsilon1 on y1 0, 1,2, 3 periods after the shock
#first graph in slide 74
theta_0[1,1]
theta_1[1,1]
theta_2[1,1]
theta_3[1,1]

#for instance, element [1,3] from theta_0,theta_1,theta_2,theta_3 gives us, respectively, the effect of epsilon3 on y1 0, 1,2, 3 periods after the shock
#third graph in slide 74
theta_0[1,3]
theta_1[1,3]
theta_2[1,3]
theta_3[1,3]

## 5.3 Orthogonalized - Forecast error decomposition----

#compare the value with the figure of slide 79 for h=1 and h=2


#for the first variable in the system, y_1,t, forecast error
#shock in epsilon 1
#h=1
cont_eps1_y1_h1=(theta_0[1,1]^2)/(theta_0[1,1]^2+theta_0[1,2]^2+theta_0[1,3]^2)
#h=2
cont_eps1_y1_h2=(theta_0[1,1]^2+theta_1[1,1]^2)/(theta_0[1,1]^2+theta_0[1,2]^2+theta_0[1,3]^2+theta_1[1,1]^2+theta_1[1,2]^2+theta_1[1,3]^2)



#for the second variable in the system, y_2,t, forecast error
#shock in epsilon 1
#h=1
cont_eps1_y2_h1=(theta_0[2,1]^2)/(theta_0[2,1]^2+theta_0[2,2]^2+theta_0[2,3]^2)
#h=2
cont_eps1_y2_h2=(theta_0[2,1]^2+theta_1[2,1]^2)/(theta_0[2,1]^2+theta_0[2,2]^2+theta_0[2,3]^2+theta_1[2,1]^2+theta_1[2,2]^2+theta_1[2,3]^2)


#for the third variable in the system, y_3,t, forecast error
#shock in epsilon 1
#h=1
cont_eps1_y3_h1=(theta_0[3,1]^2)/(theta_0[3,1]^2+theta_0[3,2]^2+theta_0[3,3]^2)
#h=2
cont_eps1_y3_h1=(theta_0[3,1]^2+theta_1[3,1]^2)/(theta_0[3,1]^2+theta_0[3,2]^2+theta_0[3,3]^2+theta_1[3,1]^2+theta_1[3,2]^2+theta_1[3,3]^2)

#shock in epsilon 2
#h=1
cont_eps2_y3_h1=(theta_0[3,2]^2)/(theta_0[3,1]^2+theta_0[3,2]^2+theta_0[3,3]^2)
#h=2
cont_eps2_y3_h2=(theta_0[3,2]^2+theta_1[3,2]^2)/(theta_0[3,1]^2+theta_0[3,2]^2+theta_0[3,3]^2+theta_1[3,1]^2+theta_1[3,2]^2+theta_1[3,3]^2)




## 6. Structural VAR representation ----
### 6.1. Using Choleski decomposition----

#Slide 80


D_1_2=diag(3)
diag(D)=diag(P2)
D_1_2 <- matrix(D, nrow = 3, ncol = 3,byrow=TRUE)
W=P2%*%solve(D_1_2)
W%*%D_1_2==P2

#slide 71
#P=W*D_1_2



#Structural VAR representation
#Slide 80
#A=W^-1
AA=solve(W) #A
B0=AA%*%v
B1=AA%*%A1
B=D_1_2

























# ................................................................................................----


# VAR(1)  with 2 variables: K=2 and p=2 ----

#STABLE VECTOR AUTOREGRESSIVE PROCESSES - Chapter 2 Lütkepohl
# VAR model
#we have to define the VAR order p and the number of variables K
#Let us assume that K=2 and p=2


##removes all the R objects
rm(list = ls())



## 1.Define the parameters and var/cov of errors matrices  ----


#1.Define the matrix of parameters A1 and A2
A1 <- c(0.5,0.1,0.4,0.5)
A1 <- matrix(A1, nrow = 2, ncol = 2,byrow=TRUE)
A2<- c(0,0,0.25,0)
A2 <- matrix(A2, nrow = 2, ncol = 2,byrow=TRUE)

#2.Define the vector of constants v
v=c(0.02,0.03)
v <- matrix(v, nrow = 2, ncol =1,byrow=TRUE)

#3.Define the variance covariance matrix of u
sigma_u=c(0.09,0,0,0.04)
sigma_u<- matrix(sigma_u, nrow = 2, ncol = 2,byrow=TRUE)

## 2.Stationarity and Stability Conditions ----

### 2.1. See if VAR is sationary -  Proposition Slide 4 Ch2----
#VAR(1) is stationary and stable the roots of |I_K-A1z| are outside the unir circle

#get matrix A from A1 and A2 ___ A=[A1 A2 IK 0] Slide 19
zeros=matrix(c(0,0,0,0),nrow=2,ncol=2)

A=matrix(rbind(cbind(A1,A2),cbind(diag(2),zeros)),nrow=4,ncol=4)

#or if the eigenvalues of matrix A are inside the unit circle
#we will use the eigenvalues of A
eig=eigen(A)
eig$values
abs(eig$values)<1


### 2.2. Expected value----
#(I_K-A1)^-1*v
mu.y=solve(diag(2)-A1-A2)%*%v
mu.y

### 2.3. Autocovariance functions----
#vec(Gamma_y(0)) ----slide 11 
Ak=kronecker(A,A)

sigma_uA=matrix(rbind(cbind(sigma_u,zeros),cbind(zeros,zeros)),nrow=4,ncol=4)
# A is the matrix for the VAR(1) representation of a VAR(2) model
# see page 29  Lutkepohl 
gam.0_VAR1=solve(diag(16)-Ak)%*%vec(sigma_uA)
gam.0_VAR1=matrix(gam.0_VAR1,nrow=4,ncol=4)
#the gam.0_VAR1 matrix contains autocavariances of order 0 and 1, gamma 0 and gamma 1 of yt
#then, from lutkepohl, page 45 pdf, we can obtain gamma_y(0) and 
#gamma_y(1) from gam.0_VAR1
gam.0=gam.0_VAR1[1:2,1:2]
gam.1=gam.0_VAR1[1:2,3:4]



#now to get back to VAR(2) representation and use A1 and A2 consider the formula in slide 17 for autocovariance matrices of yt

#Slide 17
# Gamma(h)=A1 * Gamma(h-1)+A2*Gamma(h-2)
gam.2=A1%*%gam.1+A2%*%gam.0
gam.3=A1%*%gam.2+A2%*%gam.1






### 2.4. Autocorrelation functions----

#D^-1*A1^h*Gamma(0)*D^-1
#slide 11: D is the square root of the diagonal of Gamma(0),variance covariance matrix of y

D=diag(sqrt(diag(gam.0)))
Di=solve(D)   
R.0=Di%*%gam.0%*%Di
R.1=Di%*%gam.1%*%Di
R.2=Di%*%gam.2%*%Di
R.3=Di%*%gam.3%*%Di


### 2.5. VMA representation----
#use the general formula in Slide 21 to get Phi_i from A1 
# for a VAR 2 model : Phi_i=Phi_i-1 *A1+Phi_i-2 *A2

Phi.1=A1
#since Phi_0=I_K
#note that A_i=0 for i>p. in this case, for i>2, since the process is VAR(2)
Phi.2=Phi.1%*%A1+diag(2)%*%A2
Phi.3=Phi.2%*%A1+Phi.1%*%A2

## 3. Forecasting ----
#see slide 28- the optimal forecast is the expected value  conditional on the information available at time T 
#see slide 42- general forma to optain forecasts h-steps ahead-

### 3.1. Point forecast----

#vector y_T
y_T=c(0.06,0.03)
y_T <- matrix(y_T, nrow = 2, ncol =1,byrow=TRUE)

#vector y_T-1
y_T_1=c(0.055,0.03)
y_T_1 <- matrix(y_T, nrow = 2, ncol =1,byrow=TRUE)

#1-step ahead optimal forecast y_{T+1}
#Slide 40
y_h1=v+A1%*%y_T+A2%*%y_T_1
#2-step ahead optimal forecast y_{T+2}

#Slide 41
y_h2=v+A1%*%y_h1+A2%*%y_T
##Slide 42 to has the general formula for the optimal h-step ahead forecasts


### 3.2. MSE matrices----
# see general formula in slide 44
#we need Phi_i computed in 2.5 (VMA representation) and sigma_u defined in line 242
MSE_h1=sigma_u
MSE_h2=sigma_u+Phi.1%*%sigma_u%*%t(Phi.1)
MSE_h3=sigma_u+Phi.1%*%sigma_u%*%t(Phi.1)+Phi.2%*%sigma_u%*%t(Phi.2)

### 3.3. Forecast intervals----
#y_t=(y_1t,y_2t,y_3t)

#see slide 37 to construct the CI
#confidence interval for y1t at 95%
#CI.lb - confidence interval lower bound
CI.lb_y1_h1=y_h1[1]-1.96*(MSE_h1[1,1])^0.5
#CI.ub - confidence interval upper bound
CI.ub_y1_h1=y_h1[1]+1.96*(MSE_h1[1,1])^0.5
#h=2
CI.lb_y1_h2=y_h2[1]-1.96*(MSE_h2[1,1])^0.5
CI.ub_y1_h2=y_h2[1]+1.96*(MSE_h2[1,1])^0.5

#confidence interval for y2t at 95%
#CI.lb - confidence interval lower bound
CI.lb_y2_h1=y_h1[2]-1.96*(MSE_h1[2,2])^0.5
#CI.ub - confidence interval upper bound
CI.ub_y2_h1=y_h1[2]+1.96*(MSE_h1[2,2])^0.5
#h=2
CI.lb_y2_h2=y_h2[2]-1.96*(MSE_h2[2,2])^0.5
CI.ub_y2_h2=y_h2[2]+1.96*(MSE_h2[2,2])^0.5


### 3.4. Forecast limit----
#see slide 33
# as h tends to infinity the forecasts tend to the expected value of y
lim_y_T_h_inf=solve(diag(2)-A1-A2)%*%v

### 3.5. MSE matrices limit----
# slide 35- for h large MSE of the forecast tends to the covariance matrix of y gam.0 in line 71
lim_MSE_h_inf=gam.0 ##obtained in line 277


## 4. Causality ----
### 4.1. Granger Causality----
#slide 57
# in order to check whether Granger causality we have to check relevant restrictions on the A1 matrix 
#y_t=(y_1t,y_2t)


if (A1[1,2]==0&A2[1,2]==0) {
  print("y_2t does not Granger- causes y_1t")
  
} else {
  print("y_2t does Granger- causes y_1t")
}



if (A1[2,1]==0&A2[2,1]==0) {
  print("y_1t does not Granger- causes y_2t")
  
} else {
  print("y_1t does Granger- causes y_2t")
}

### 4.2. Multi-step Causality----
# we saw above that information in y_2t cannot be used to improve the 1-step ahead forecast of y_1T
#however, it is still possible that it can be used to improve the h-step forecasts, h=2,...
#multi-step casuality
#a concept which refer explicitly to the forecast horizon

# 1-step casuality

if (A1[1,2]==0) {
  print("y_2t is not 1-step ahead causal for y_1t ")
  
} else {
  print("y_2t is 1-step ahead causal for y_1t ")
}

# 2-step casuality
A1[1,2]==0
#if it is true, y_2t is not 1-step ahead causal for y_1t 

if (A2[1,2]==0) {
  print("y_2t is not 2-step ahead causal for y_1t ")
  
} else {
  print("y_2t is 2-step ahead causal for y_1t ")
}





#infinite step noncasuality requires that the relevant elements of A1 and A1^2
#for a VAR(2) process we only have to check A, A^2 and A^3  for the VAR(1) representation

A_2=A%*%A
A_3=A_2%*%A

if (A[1,2]==0&A_2[1,2]==0&A_3==0) {
  print("y_2t is not infinite-step ahead causal for y_1t ")
  
} else {
  print("y_2t is infinite-step ahead causal for y_1t ")
}
