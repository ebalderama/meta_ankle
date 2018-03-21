#here's the log likelihood function
loglike<-function(Y,N,XB){
  #XB is rate (defined in next function), N is offset (What is Y, then?)
  #"lam" for "lambda" = poisson parameter
  lam <- N*exp(XB)
  sum(dpois(Y,lam, log=TRUE))
}

#here's the mcmc
#X is... a matrix of... parameters? Make sure the first column is ones!
#Y is... whatever result we're looking at. Overall failure or complication rate, for example?
#N is the offset term. Aka, number of ankles in this case
poisMCMC <- function(Y,N,X){

  #p for number of parameters, I think (in this case # of dummy variables for prosthesis type)
  p <- ncol(X)
  
  #new - we need to say n.iters for later looping, etc. Also, prior_beta_mn, prior_beta_sd
  n.iters<-4000
  prior_beta_mn<-0
  prior_beta_sd<-1
  
  #ok, we already have p - what are/where do we specify prior beta mean & standard deviation?
  #if left out, they default to 0, but I'm not sure if that's valid (and clearly var names are inserted)
  #beta will be p elements long
  beta <-rnorm(p,prior_beta_mn,prior_beta_sd)
  XB <- c(X %*% beta)
  lam <- N*exp(XB)
  
  #current log likelihood
  #make sure X has a column of ones
  #wait... X, or XB?
  curll <- loglike(Y,N,X)
  
 
  keep.beta <-matrix(NA,n.iters,p)
  #first row are initial values (1s?)
  keep.beta[1,] <- beta
  keep.ll <-curll
  
  tune_beta <- 1
  
  for(iter in 2:n.iters){
    #update beta; p is the number of dummy vars? (in this case)
    for(j in 1:p){
      canbeta <- rnorm(1,beta[j],tune_beta)
      canXB <- XB + X[,j]*(canbeta-beta[j])
      canll <- loglike(Y,N,canXB)
      
      R <- canll + dnorm(canbeta,prior_beta_mn,prior_beta_sd,log=TRUE) - 
        curll + dnorm(beta[j],prior_beta_mn, prior_beta_sd,log=TRUE)
      
      if(log(runif(1))<R){
        beta[j] <- canbeta
        XB <-canbeta
        curll <- canll
      }
    }
    
    #keep track of stuff
    keep.beta[iter,] <- beta
    keep.ll[iter]<-curll
    
    #tuning
    #ok, what are acc & att?
    #here are some possibilities...?
    acc<-curll
    att<-canll
    #why are we changing the tuning parameter?
    if(iter%%50==0){
      #The commented version is from last week
      #tune_beta<-ifelse(acc/att < 0.20,0.8*tune_beta,1.2*tune_beta)
      tune_beta<-ifelse(acc/att < 0.20,0.8*tune_beta,1.2*tune_beta)
    }
    
    #plot
    if(iter%%50==0){
      par(mfrow=c(2,2))
      #the following (commented) line was in addition to the line below it: identicle minus the "if"
      #Both versions were there from last week.
      #plot(keep.beta[1:iter,1],type="s",ylab="",main=bquote(beta[0]))
      if(p>1) plot(keep.beta[1:iter,1],type="s",ylab="",main=bquote(beta[0]))
      plot(keep.ll[1:iter],type="s",ylab="",main="log-likelihood")
      }
  }
  #output
  list(beta=keep.beta,ll=keep.ll)
}

#Let's try it!!!
poisMCMC(Y=comp.ovr,N=n,X=cbind(rep(1,38),taarows,aaorows,aaarows))