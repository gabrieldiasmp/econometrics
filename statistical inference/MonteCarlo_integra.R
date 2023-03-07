#
# Monte Carlo Integration:
#     h(x) = (cos(50*x)+sin(20*x))^2, 0<x<1
#
h <- function(x) {(cos(50*x)+sin(20*x))^2}
set.seed(1234)
n <- 10^4    # num of MC sim
u <- runif(n)

par(mar=c(2,4,2,4),mfrow=c(2,1)) #plots
curve(h,0,1,main="h(x)=[cos(50x)+sin(20x)]^2")

est_int <- cumsum(h(u))/(1:n)
est_int[n]
est_err <- sqrt(cumsum((h(u)-est_int)^2))/(1:n)
plot(est_int,type="l",lwd=2,main="Mean and error as a function of n")
lines(est_int+2*est_err, col="green",lwd=2)
lines(est_int-2*est_err, col="green",lwd=2)

# true value of the integral of h(u)
integrate(h,0,1)

