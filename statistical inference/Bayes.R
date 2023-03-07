# Bayesian statistics: graphical representation of the prior, likelihood and posterior
#     X ~ Ex(theta)
#     in a sample o size n=5 we observed sum(x)=2.7
# theta ~ Gamma(alpha,delta), with alpha=8 and delta=3
# theta | x ~ Gamma(alpha+n, sum(x)+delta)
#
n <- 5
sx <- 2.7
alpha <- 8
delta <- 3
Lik <- function(theta) {theta^n*exp(-theta*sx)}
curve(dgamma(x,shape=alpha,scale=1/delta), from=0, to=8, col="blue", ylim=c(0,0.7))
abline(v=alpha/delta, col="blue")
curve(Lik(x), from=0, to=8, add=T, col="red")
abline(v=n/sx, col="red")
curve(dgamma(x,shape=alpha+n,scale=1/(sx+delta)), from=0, to=8, add=T)
abline(v=(alpha+n)/(sx+delta))
legend("topright", legend=c("Prior","Likelihood","Posterior"),col=c('blue','red','black'),lty=c(1,1,1))

# estimates
theta.ML <- n/sx                   # ML esrimate
theta.0 <- alpha/delta             # prior mean
theta.1 <- (alpha+n)/(sx+delta)    # posterior mean
c(theta.ML,theta.0,theta.1)
