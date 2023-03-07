# Binomial
par(mfrow=c(3,2))
n <- 5; x <- 0:n; p <- .05
plot(x,dbinom(x,n,p),ylab="p(x)",main="Binomial(n=5, p=0.05)")
n <- 5; x <- 0:n; p <- .5
plot(x,dbinom(x,n,p),ylab="p(x)",main="Binomial(n=5, p=0.5)")
n <- 20; x <- 0:n; p <- .05
plot(x,dbinom(x,n,p),ylab="p(x)",main="Binomial(n=20, p=0.05)")
n <- 20; x <- 0:n; p <- .5
plot(x,dbinom(x,n,p),ylab="p(x)",main="Binomial(n=20, p=0.5)")
n <- 80; x <- 0:n; p <- .05
plot(x,dbinom(x,n,p),ylab="p(x)",main="Binomial(n=80, p=0.05)")
n <- 80; x <- 0:n; p <- .5
plot(x,dbinom(x,n,p),ylab="p(x)",main="Binomial(n=80, p=0.5)")
par(mfrow=c(1,1))

# Binomial(4,0.4):
# P(X>=1)
1-dbinom(0,4,.4)
dbinom(1,4,.4)+dbinom(2,4,.4)+dbinom(3,4,.4)+dbinom(4,4,.4)
1-pbinom(0,4,.4)
#P(X>=2)
1-dbinom(0,4,.4)-dbinom(1,4,.4)
dbinom(2,4,.4)+dbinom(3,4,.4)+dbinom(4,4,.4)
1-pbinom(1,4,.4)
#P(X<=2)
dbinom(0,4,.4)+dbinom(1,4,.4)+dbinom(2,4,.4)
pbinom(2,4,.4)
#P(X<2)
dbinom(0,4,.4)+dbinom(1,4,.4)
pbinom(1,4,.4)
#P(X=1)
dbinom(1,4,.4)
#P(1<X<=3)
dbinom(2,4,.4)+dbinom(3,4,.4)

# Exponencial
x <- seq (0,2,length=40)
lam <- c (2,1,.2,.1)
y <- matrix (NA,40,4)
for (i in 1:4)
	y[,i] <- dexp(x, 1/lam[i])
matplot(x,y,type="l",xlab="x",ylab="p(x)",col=1)
legend(1.2,10,paste("lambda=", lam),lty=1:4,cex=.75)
title("Exponencial")

# Normal
x <- seq(-6,6,len=100)
y <- cbind(dnorm(x,-2,1),
           dnorm(x,0,2),
           dnorm(x,0,.5),
           dnorm(x,2,.3),
           dnorm(x,-.5,3)
           )
matplot(x,y,type="l",col=1:5)
legend(-6,1.3,paste("mu=",c(-2,0,0,2,-.5),
                    ";sigma=",c(1,2,.5,.3,3)),
        lty=1:5,col=1:5,cex=.75)
title("Normal")        

# Gama
# f(x)= 1/(s^a Gamma(a)) x^(a-1) e^-(x/s)
# scale=s, shape=a
x <- seq(0,12,len=100)
y <- cbind(dgamma(x,1,1),
           dgamma(x,1,2),
           dgamma(x,.5,.5),
           dgamma(x,.5,.2),
           dgamma(x,2,3)
           )
matplot(x,y,type="l",col=1:5)
legend(8,2,paste("alpha=",c(1,1,.5,.5,2),
                    ";beta=",c(1,2,.5,.2,3)),
        lty=1:5,col=1:5,cex=.75)
title("Gama")


# Beta
x <- seq(0,1,len=100)
y <- cbind(dbeta(x,1,1),
           dbeta(x,1,2),
           dbeta(x,2,1),
           dbeta(x,2,3),
           dbeta(x,.4,.6)
           )
matplot(x,y,type="l",col=1:5)
legend(.65,4.5,paste("alpha=",c(1,1,2,2,.4),
                    ";beta=",c(1,2,1,3,.6)),
        lty=1:5,col=1:5,cex=.75)
title("Beta")



# Weibull
# f(x)= (a/b)(x/b)^(a-1)exp(-(x/b)^a)
# scale=b, shape=a
x <- seq(0,8,len=100)
y <- cbind(dweibull(x,1,1),
           dweibull(x,1,2),
           dweibull(x,.5,.5),
           dweibull(x,.5,.2),
           dweibull(x,2,3)
           )
matplot(x,y,type="l",col=1:5,main="Weibull")
legend(5,2,paste("alpha=",c(1,1,.5,.5,2),
                    ";beta=",c(1,2,.5,.2,3)),
        lty=1:5,col=1:5,cex=.75)


# Weibull
# f(x)= (a/b)(x/b)^(a-1)exp(-(x/b)^a)
# scale=b, shape=a
par(mfrow=c(2,2))
x <- seq(0,8,len=100)
plot(x,dweibull(x,1,1),type="l",ylab="pdf",main="Wibull(1,1)")
plot(x,dweibull(x,1,2),type="l",ylab="pdf",main="Wibull(1,2)")
plot(x,dweibull(x,.5,.5),type="l",ylab="pdf",main="Wibull(.5,.5)")
plot(x,dweibull(x,2,3),type="l",ylab="pdf",main="Wibull(2,3)")
par(mfrow=c(1,1))