# Monte Carlo simulation to study the properties of the
# sum of two exponential rv, with distributions,
#   X ~ Ex(a) e Z ~ Ex(a)
#   with a=2
# It is intended to calculate, by MC simulation, the mean
# and the variance of the rv, W=X+Z
# analytically it is known that W ~ G(2,a) with moments,
# E(W)=2/a and Var(W)=2/a^2
# 
# p1 = Prob(W<1)
#

# parameter value
a <- 2

# MC simulation
set-seed(123)
n <- 1
sim <- 20000
w <- matrix(NA,sim,1)
p1 <- matrix(NA,sim,1)
for(i in 1:sim){
	x <- rexp(n,rate=a)
	z <- rexp(n,rate=a)
	w[i] <- x+z
	p1[i] <- w[i]<1
}
hist(w,breaks=100,probability=TRUE)
curve(dgamma(x,shape=2,rate=a),add=TRUE, col="red")
mean(w)   # Appr. mean(W)
var(w)    # Appr. Var(W)
pgamma(1,shape=2,rate=a)  # True  Pr(W<1)
mean(p1)                  # Appr. Pr(W<1)


