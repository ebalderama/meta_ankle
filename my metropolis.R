#set.seed(1234)
set.seed(.Random.seed)


#likelihood
loglike <- function(failx,theta){
  p<-1/(1+exp(-theta))
  sum(log(dbinom(failx,total,p))) #d is for density
}

logprior <- function(theta){
  log(dnorm(theta,mu0,sigma0))
}

# logsigprior <- function(sig){
#   log(dnorm(sig,sigma0,sigsigma0))
# }

#prior(s)
mu0<- 0
sigma0 <- 100
#sigsigma <- 100


#Initial values
theta <- 0
tune_sd <- 1
#sig <- 0
#tune_ssd <- 1

#Bookkeeping
n.iters <- 5000
keep.theta <- rep(theta, n.iters)

#start mcmc
for (iter in 2:n.iters){
  #update theta
  
  #1. propose a candidate
  cantheta <- rnorm(1,theta,tune_sd)
  
  #2. compute acceptance ratio (R is for "ratio")
  #R <- like(failx, cantheta)*prior(cantheta)/(like(failx,theta)*prior(theta))
  logR <- (loglike(failx, cantheta)+logprior(cantheta))-(loglike(failx,theta)+logprior(theta))
  #3. make a decision
  if(log(runif(1)) < logR){
    theta <- cantheta
  }
  
  #keep track of results
  keep.theta[iter] <-theta
}

burn<-1:500
keep.theta <- keep.theta[-burn]
plot(keep.theta,type="s",xlab="Iteration",ylab=bquote(theta),main="Trace plot")

hist(keep.theta,breaks=50,main=bquote(bold("Posterior of"~theta)))

p<-1/(1+exp(-keep.theta))
hist(p,breaks=50,main="posterior of p")

summary(p); sd(p)

##diagnostics
library(coda)
chain<-mcmc(keep.theta)
effectiveSize(chain)

autocorr.plot(chain)

#maybe thin by 10?
thinned.theta <- keep.theta[seq(1,n.iters,10)]
length(thinned.theta)

thinned.chain <- mcmc(thinned.theta)
#This is giving me "Error in na.fail.default(as.ts(x)) : missing values in object
effectiveSize(thinned.chain)
#So is this.
autocorr.plot(thinned.chain)

thinned.p <- 1/(1+exp(-thinned.theta))
hist(thinned.p,breaks=50,main="Posterior of p")

#Where are the NAs!?
summary(thinned.p); sd(thinned.p)

######################################################################################
#THIS IS THE VERSION WITH "IMPROVED" PRIORS/INTIALVALUES
#prior
mu0<-(-2.5126)
sigma0 <- 100

#Initial values
theta <- -2
tune_sd <- 1

#Bookkeeping...
n.iters <- 5000
keep.theta <- rep(theta, n.iters)

#start mcmc
for (iter in 2:n.iters){
  #update theta
  
  #1. propose a candidate
  cantheta <- rnorm(1,theta,tune_sd)
  
  #2. compute acceptance ratio (R is for "ratio")
  logR <- (loglike(failx, cantheta)+logprior(cantheta))-(loglike(failx,theta)+logprior(theta))
  
  #3. make a decision
  if(log(runif(1)) < logR){
    theta <- cantheta
  }
  
  #keep track of results
  keep.theta[iter] <-theta
}

plot(keep.theta,type="s",xlab="Iteration",ylab=bquote(theta),main="Trace plot")

hist(keep.theta,breaks=50,main=bquote(bold("Posterior of"~theta)))

p<-1/(1+exp(-keep.theta))
hist(p,breaks=50,main="posterior of p")

summary(p); sd(p)

##diagnostics
library(coda)
chain<-mcmc(keep.theta)
effectiveSize(chain)

autocorr.plot(chain)

#maybe thin by 10?
thinned.theta <- keep.theta[seq(1,n.iters,10)]
length(thinned.theta)

thinned.chain <- mcmc(thinned.theta)
effectiveSize(thinned.chain)

autocorr.plot(thinned.chain)

thinned.p <- 1/(1+exp(-thinned.theta))
hist(thinned.p,breaks=50,main="Posterior of p")

summary(thinned.p); sd(thinned.p)

