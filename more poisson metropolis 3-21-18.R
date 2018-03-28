#here's the log likelihood function
loglike<-function(Y,N,XB){
  #XB is rate (defined in next function), N is offset, Y is what we're looking at
  #"lam" for "lambda" = poisson parameter
  #lam <- N*exp(XB)
  loglam<-log(N)+XB
  sum(dpois(Y,exp(loglam), log=TRUE))
}

#here's the mcmc
#X is... a matrix of... parameters? Make sure the first column is ones!
#Y is... whatever result we're looking at. Overall failure or complication rate, for example?
#N is the offset term. Aka, number of ankles in this case
poisMCMC <- function(Y,N,X=NULL,n.iters=4000,prior_beta_mn=0,prior_beta_sd=100, burn=0.5*n.iters){


  
  if(is.null(X)) X<-matrix(1,length(Y),1)
  #p for number of parameters, I think (in this case # of dummy variables for prosthesis type)
  p <- ncol(X)
  
  #ok, we already have p - what are/where do we specify prior beta mean & standard deviation?
  #if left out, they default to 0, but I'm not sure if that's valid (and clearly var names are inserted)
  #beta will be p elements long
  #beta <-rnorm(p,prior_beta_mn,prior_beta_sd)
  beta <- rep(0,p)
  XB <- X %*% beta
  
  mis<-anyNA(Y)
  
  if(mis) {
    m<-which(is.na(Y))
    Y[m]<-rpois(length(m),exp(log(N[m])+XB[m]))
    keep.ymis<-matrix(NA,n.iters,length(m))
  }
  
  #current log likelihood
  #make sure X has a column of ones
  curll <- loglike(Y,N,XB)
  
 
  keep.beta <-matrix(NA,n.iters,p)
  #first row are initial values
  keep.beta[1,] <- beta
  keep.ll <-curll
  
  tune_beta <- rep(1,p)
  acc=rep(0,p)
  att=0
  
  for(iter in 2:n.iters){
    #update beta; p is the number of dummy vars (in this case)
    
    att<-att+1
    
    for(j in 1:p){
      
      canbeta <- rnorm(1,beta[j],tune_beta)
      canXB <- XB + X[,j]*(canbeta-beta[j])
      canll <- loglike(Y,N,canXB)
      
      R <- canll + dnorm(canbeta,prior_beta_mn,prior_beta_sd,log=TRUE) - 
        curll - dnorm(beta[j],prior_beta_mn, prior_beta_sd,log=TRUE)
      
      if(log(runif(1))<R){
        acc[j]<-acc[j]+1
        beta[j] <- canbeta
        XB <- canXB
        curll <- canll
      }
    }
    
    if(mis) {
      Y[m]<-rpois(length(m),exp(log(N[m])+XB[m]))
      #keep track of missing Ys
      keep.ymis[iter,]<-Y[m]
    }
    
    #keep track of stuff
    keep.beta[iter,] <- beta
    keep.ll[iter]<-curll
    
    #tuning
    if(att%%50==0 & iter<0.5*burn){
      tune_beta<-ifelse(acc/att < 0.20,0.8*tune_beta,tune_beta)
      tune_beta<-ifelse(acc/att > 0.60,1.2*tune_beta,tune_beta)
      acc<- 0*acc
      att<-0
    }
    
    #plot
    if(iter%%50==0){
      par(mfrow=c(2,2))
      #the following (commented) line was in addition to the line below it: identical minus the "if"
      #Both versions were there from last week.
      plot(exp(keep.beta[1:iter,1]),type="s",ylab="",main=bquote(exp(beta[0])))
      if(p>1) plot(exp(keep.beta[1:iter,2]),type="s",ylab="",main=bquote(exp(beta[1])))
      if(p>2) plot(exp(keep.beta[1:iter,3]),type="s",ylab="",main=bquote(exp(beta[2])))
      plot(keep.ll[1:iter],type="s",ylab="",main="log-likelihood")
      }
  }
  #output
  output <- list(beta = keep.beta[-(1:burn),],
       ll   = keep.ll[-(1:burn)], 
       mu   = N*exp(XB)
       )
  if(mis) output$ymis= keep.ymis[-(1:burn),]
  return(output)
}

#Let's try it!!!
throwaway<-counts345[[10]]
throwaway[c(1,24,33)]<-NA
#run<-poisMCMC(Y=counts345[[10]],N=n,X=cbind(rep(1,38),as.numeric(aaorows),as.numeric(aaarows)),n.iters=5000)
#run2<-poisMCMC(Y=counts345[[10]],N=n,X=cbind(rep(1,38),as.numeric(taarows),as.numeric(aaarows)),n.iters=10000)
#run3<-poisMCMC(Y=counts345[[9]],N=n,X=cbind(rep(1,38),as.numeric(aaorows),as.numeric(aaarows)),n.iters=10000)
#run3<-poisMCMC(Y=counts345[[9]],N=n,n.iters=10000)
run2<-poisMCMC(Y=throwaway,N=n,X=cbind(rep(1,38),as.numeric(taarows),as.numeric(aaarows)),n.iters=10000)

#summary(run$beta)
#summary(exp(run$beta))

summary(run2$beta)
summary(exp(run2$beta))

#posterior distributions of missing data
summary(run2$ymis)
