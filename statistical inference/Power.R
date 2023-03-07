#
# Power function
#
# H0: mu = 1.5 (mu0)
# H1: mu > 1.5 (mu1)
#
# alpha =0.05
#
# X ~ N(mu,1), n=10 and x_bar = 1.4 (observed value)
#

mu <- 1.5
sig <- 1
n <- 10
xc <- mu + qnorm(0.05,lower.tail=F)*sig/sqrt(n)

beta <- function(mu1){
  pnorm((xc-mu1)/(sig/sqrt(n)),lower.tail=F)
}

curve(beta(x),from=1.5,to=3)


# How does the sample size affect the power?
beta <- function(mu1,n){
  pnorm((mu + qnorm(0.05,lower.tail=F)*sig/sqrt(n)-mu1)/(sig/sqrt(n)),lower.tail=F)
}
curve(beta(x,10),from=1.5,to=3)
for(i in c(20,50,100,200)){
  curve(beta(x,i),from=1.5,to=3, add=T)
}