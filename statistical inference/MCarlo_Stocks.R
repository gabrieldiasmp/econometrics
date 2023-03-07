# Monte Carlo simulation:
# A retailer sells a perishable commodity 
# and each day he places an order for Q units. 
# Each unit that is sold gives a profit of 60 
# cents and units not sold, at the end of the 
# day, are discarded at a loss of 40 cents per 
# unit. The demand (D) on any given day is
# uniformly distributed on [80; 140]. How many 
# units should the retailer order to maximize
# expected profit (EP)?
#

Q <- matrix(80:140,nrow=61,ncol=1)
nQ <- nrow(Q)
n <- 100000

EP <- matrix(NA,nQ,1)
set.seed(123)
for(j in 1:nQ){
	D <- runif(n,min=80,max=140)
	P <- 0.6*Q[j]*(D>=Q[j]) + (0.6*D-0.4*(Q[j]-D))*(Q[j]>D)
	EP[j] <- mean(P)	
}
plot(Q,EP, type="l",xlab="Quantities",ylab="Expected Profit")
matrix(c(Q,EP),nrow=61,ncol=2)
max(EP)
Q[EP==max(EP)]
