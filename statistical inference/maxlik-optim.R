#
# Maximum likelihood estimation (optim)
#

## Example 1
## Binomial: tossing a coin 20x

NLL.Bin1 <- function(theta, y) {
	NLL.Bin1 <- 0
	N <- length(y)
	for (i in 1:N) {
		if (y[i] == 1) {p <- theta}     # cara
		if (y[i] == 0) {p <- 1-theta}   # coroa
		NLL.Bin1 <- NLL.Bin1 + log(p)
	}
	-NLL.Bin1
}

flips <- c(1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0)  # 12 heads e 8 tails
out.Bin1 <- optim(par=0.5, fn=NLL.Bin1, method="Brent", lower=0.00001, upper=0.9999, y=flips)
out.Bin1$par
out.Bin1$value

NLL.Bin2 <- function(p, y) {
	-sum(stats::dbinom(x=y, size=1, prob=p, log=TRUE))
}

out.Bin2 <- optim(par=0.5, fn=NLL.Bin2, method="Brent", lower=0.00001, upper=0.9999, y=flips)
out.Bin2$par
out.Bin2$value


## Example 2
## Normal

NLL.Norm1 <- function(theta, data) {
	mu <- theta[1]
	sg <- theta[2]
	n  <- length(data)
	NLL.Norm1 <- -(n/2)*log(2*pi) - (n/2)*log(sg**2)
	tmp <- 0
	for (i in 1: n) {
		tmp <- tmp + (data[i]-mu)**2
	}
	NLL.Norm1 <- NLL.Norm1 + -(1/(2*(sg**2)))*tmp
	-NLL.Norm1
}

x <- c(85,84,75,93,88,82,85,94,86,76,81,98,95,82,76,91,81,82,72,94)
out.Norm1 <- optim(par=c(100,10), fn=NLL.Norm1, data=x)
out.Norm1$par
out.Norm1$value

NLL.Norm2 <- function(theta, data) {
	mu <- theta[1]
	sg <- theta[2]
	n  <- length(data)
	NLL.Norm2 <- -(n/2)*log(2*pi) - (n/2)*log(sg**2) - (1/(2*sg**2))*sum((data-mu)**2)
	-NLL.Norm2
}

out.Norm2 <- optim(par=c(100,10), fn=NLL.Norm2, data=x)
out.Norm2$par
out.Norm2$value


## Example 3
## Poisson

NLL.Pois1 <- function(theta, data) {
	n  <- length(data)
	tmp1 <- 0
	tmp2 <- 0
	for (i in 1: n) {
		tmp1 <- tmp1 + data[i]
		tmp2 <- tmp2 + lfactorial(data[i])
	}
	NLL.Pois1 <- -n*theta + tmp1*log(theta) - tmp2
	-NLL.Pois1
}

y <- rpois(10000,2)
out.Pois1 <- optim(par=5, fn=NLL.Pois1, method="Brent", lower=0.00001, upper=10, data=y)
out.Pois1$par
out.Pois1$value

NLL.Pois2 <- function(theta, data) {
	n  <- length(data)
	NLL.Pois2 <- -n*theta + sum(data)*log(theta) - sum(lfactorial(data))
	-NLL.Pois2
}

out.Pois2 <- optim(par=5, fn=NLL.Pois2, method="Brent", lower=0.00001, upper=10, data=y)
out.Pois2$par
out.Pois2$value


NLL.Pois3 <- function(theta, data) {
  n  <- length(data)
  -sum(stats::dpois(x=data, lambda=theta, log=TRUE))
}

out.Pois3 <- optim(par=5, fn=NLL.Pois3, method="Brent", lower=0.00001, upper=10, data=y)
out.Pois3$par
out.Pois3$value

system.time(out.Pois1 <- optim(par=5, fn=NLL.Pois1, method="Brent", lower=0.00001, upper=10, data=y))
system.time(out.Pois2 <- optim(par=5, fn=NLL.Pois2, method="Brent", lower=0.00001, upper=10, data=y))
system.time(out.Pois3 <- optim(par=5, fn=NLL.Pois3, method="Brent", lower=0.00001, upper=10, data=y))


## Exampl1 4
## Gamma

NLL.Gam1 <- function(theta, data) {
  a <- theta[1]
  b <- theta[2]
  NLL.Gam1 <- sum(-a*log(b) - log(gamma(a)) + (a-1)*log(data) - data/b)
  -NLL.Gam1
}

y <- rgamma(n=1000,shape=2,scale=2)
out.Gam1 <- optim(par=c(1,1), fn=NLL.Gam1, data=y)
out.Gam1$par
out.Gam1$value


NLL.Gam2 <- function(theta, data) {
  a <- theta[1]
  b <- theta[2]
  -sum(stats::dgamma(x=data,shape=a,scale=b,log=TRUE))
}

out.Gam2 <- optim(par=c(1,1), fn=NLL.Gam2, data=y)
out.Gam2$par
out.Gam2$value


system.time(out.Gam1 <- optim(par=c(1,1), fn=NLL.Gam1, data=y))
system.time(out.Gam2 <- optim(par=c(1,1), fn=NLL.Gam2, data=y))
