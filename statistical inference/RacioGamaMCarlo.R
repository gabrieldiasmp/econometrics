# Monte Carlo simulation to study the properties
# of a function of rv  with known distributions
#   X ~ G(a,d) e Z ~ G(b,d)
#   with a=2, b=3 and d=1
# It is intended to calculate, by MC simulation, the average,
# variance and the distribution of the rv, W=X/(X+Z)
# [ analytically it is known that W ~ Beta(a,b) with moments,
#   E(W)=0.4 e Var(W)=0.04 ]
# 

# parameter values
a <- 2
b <- 3
d <- 1

# MC simulation
set.seed(123)
n <- 1
sim <- 5000
w <- matrix(NA,sim,1)
for(i in 1:sim){
	x <- rgamma(n,shape=a,scale=d)
	z <- rgamma(n,shape=b,scale=d)
	w[i] <- x/(x+z)
}
hist(w,breaks=50,probability=TRUE)
curve(dbeta(x,shape1=a,shape2=b),add=TRUE,col="red")
# Approximated values
mean(w)
var(w)

# 
# Pr(W > 0.5) = ?  where W ~ Beta(2,3)
# True Probability
pbeta(0.5,2,3,lower.tail=FALSE)
# Approximated Probability given by Monte Carlo
mean(w>0.5)

