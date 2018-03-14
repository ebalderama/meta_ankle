#Package: MCMCpack - looking at 'MCMCpoisson' function
#oh wait, I'm not sure if this is actually useful?
comp.ovr <- as.numeric(as.character(unlist(counts345[[9]])))
total <- as.numeric(n)
posterior <- MCMCpoisson(comp.ovr~total)
plot(posterior)
summary(posterior)
#mu<-exp(2.79465+0.00376*)

hist(posterior)


#Let's try some more Metropolis
set.seed(.Random.seed)

#likelihood
loglike <- function(Y,lamb){
  sum(log(dpois(Y,lamb)))
}

logprior <- function(lamb){
  log(dnorm(lamb,.01))
}

# logsigprior <- function(sig){
#   log(dnorm(sig,sigma0,sigsigma0))
# }

#prior(s)
mu0<- 10
#sigma0 <- 100
#sigsigma <- 100


#Initial values
theta <- 10
tune <- 1
#sig <- 0
#tune_ssd <- 1

#Bookkeeping
n.iters <- 5000
keep.theta <- rep(theta, n.iters)

#start mcmc
#Error in if (log(runif(1)) < logR) { : missing value where TRUE/FALSE needed
#This will be due to the missing values. Ugh.
for (iter in 2:n.iters){
  #update theta
  
  #1. propose a candidate
  cantheta <- rnorm(1,theta,tune)
  
  #2. compute acceptance ratio (R is for "ratio")
  #R <- like(failx, cantheta)*prior(cantheta)/(like(failx,theta)*prior(theta))
  logR <- (loglike(rev.to.fus, cantheta)
           +logprior(cantheta))-(loglike(rev.to.fus,theta)
                                            +logprior(theta))
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
#library(coda)
#chain<-mcmc(keep.theta)
#effectiveSize(chain)

#autocorr.plot(chain)

#maybe thin by 10?
#thinned.theta <- keep.theta[seq(1,n.iters,10)]
#length(thinned.theta)

#thinned.chain <- mcmc(thinned.theta)
#effectiveSize(thinned.chain)
#autocorr.plot(thinned.chain)

#thinned.p <- 1/(1+exp(-thinned.theta))
#hist(thinned.p,breaks=50,main="Posterior of p")

#Where are the NAs!?
#summary(thinned.p); sd(thinned.p)
