set.seed(52992)

#here's the log likelihood function
loglike <- function(Y, N,XB,RE){
  theta<-XB+RE
  p <- 1 / (1 + exp(-theta))
  sum(dbinom(Y, N, p, log = TRUE))
}

logthetaprior <- function(XB, RE, s2){
  theta <- XB + RE
  sum(dnorm(theta, sqrt(s2), log = TRUE))
}

#here's the mcmc
#X is a matrix of parameters. Make sure the first column is ones!
N_Bmcmc <- function(Y, N, X = NULL,
                    n.iters = 4000, burn = 0.5*n.iters, update = round(n.iters/20),
                    prior_beta_mn = 0, prior_beta_sd = .1,
                    #a = 0.5, b = 0.5,
                    prior_tau_a = 1, prior_tau_b = 5
                    ){


  if(is.null(X)) X <- matrix(1, length(Y), 1)


  #=========================================
  # Initial Values
  #=========================================
  n.pars <- ncol(X)
  n <- length(Y) #number of studies
  s2 <- 1 / (Y + .1) + 1 / (N - Y) #known within study variances
  beta <- rnorm(n.pars, prior_beta_mn, prior_beta_sd)
  XB <- X %*% beta
  #tau <- rgamma(1, prior_tau_a, prior_tau_b) #between-study variance
  tau <- 1 #between-study variance
  RE <- rnorm(n, 0, tau)
  thetaprior<-logthetaprior(XB,RE,s2)
  curll <- loglike(Y, N, XB, RE) #current log likelihood



  #=========================================
  # Keepers
  #=========================================
  keep.beta <- matrix(NA, n.iters, n.pars)
  keep.beta[1, ] <- beta
  keep.tau <- tau
  keep.RE <- matrix(NA, n.iters, n)
  keep.RE[1, ] <- RE
  keep.ll  <- curll

  #=========================================
  # Tuning
  #=========================================
  tune_beta <- rep(1, n.pars)
  acc = rep(0, n.pars)
  att = 0

  tune_RE <- 1
  acc_RE <- att_RE <- 0



  #=========================================
  #=========================================
  # Start MCMC!
  #=========================================
  #=========================================
  for(iter in 2:n.iters){
    #update beta; pars is the number of dummy vars (in this case)

    att <- att + 1


    #=========================================
    # Update beta
    #=========================================
    for(j in 1:n.pars){
      canbeta <- rnorm(1, beta[j], tune_beta[j])
      canXB <- XB + X[, j]*(canbeta-beta[j])
      canthetaprior <- logthetaprior(canXB,RE,s2)
      canll <- loglike(Y, N, canXB,RE)


      R1 <- canll + canthetaprior + dnorm(canbeta, prior_beta_mn, prior_beta_sd, log = TRUE) -
        curll -thetaprior -  dnorm(beta[j], prior_beta_mn,  prior_beta_sd, log = TRUE)

      if(log(runif(1)) < R1){
        acc[j] <- acc[j] + 1
        beta[j] <- canbeta
        XB <- canXB
        thetaprior<-canthetaprior
        curll <- canll
      }
    }

    #=========================================
    # Update random effects
    #=========================================
    for(i in 1:n){
      att_RE <- att_RE + 1
      canRE <- RE
      canRE[i] <- rnorm(1, RE[i], tune_RE)
      canthetaprior<-logthetaprior(XB,canRE,s2)
      canll <- loglike(Y, N, XB, canRE)

      R2 <- canll +canthetaprior+ dnorm(canRE[i], 0, tau, log = TRUE) -
        curll - thetaprior -dnorm(RE[i], 0,  tau, log = TRUE)

      if(log(runif(1)) < R2){
        acc_RE <- acc_RE + 1
        RE <- canRE
        thetaprior<-canthetaprior
        curll <- canll
      }
    }


    #=========================================
    # Update tau
    #=========================================
    tau <- rgamma(1, n / 2 + prior_tau_a, sum(RE^2) / 2 + prior_tau_b)




    #=========================================
    # keep track of stuff
    #=========================================
    keep.beta[iter, ] <- beta
    keep.RE[iter, ] <- RE
    keep.tau[iter] <- tau
    keep.ll[iter] <- curll


    #=========================================
    # tuning
    #=========================================
    if(att%%50 == 0 & iter < 0.5*burn){
      tune_beta <- ifelse(acc / att < 0.25, 0.8*tune_beta, tune_beta)
      tune_beta <- ifelse(acc / att > 0.50, 1.2*tune_beta, tune_beta)
      acc <-  0*acc
      att <- 0
    }
    if(att_RE%%(2*n) == 0 & iter < 0.5*burn){
      tune_RE <- ifelse(acc_RE / att_RE < 0.25, 0.8*tune_RE, tune_RE)
      tune_RE <- ifelse(acc_RE / att_RE > 0.50, 1.2*tune_RE, tune_RE)
      acc_RE <- att_RE <- 0
    }

    #=========================================
    # plots
    #=========================================
    if(iter%%update == 0){
      par(mfrow = c(3, 2))
      plot(exp(keep.beta[1:iter, 1]), type = "s", ylab = "", main = bquote(exp(beta[0])))
      if(n.pars>1) plot(exp(keep.beta[1:iter, 2]), type = "s", ylab = "", main = bquote(exp(beta[1])))
      if(n.pars>2) plot(exp(keep.beta[1:iter, 3]), type = "s", ylab = "", main = bquote(exp(beta[2])))
      plot(keep.ll[1:iter], type = "s", ylab = "", main = "log-likelihood")
      plot(keep.tau[1:iter], type = "s", ylab = "", main = bquote(tau))
    }
  }


  #=========================================
  # output
  #=========================================
  output <- list(beta  =  keep.beta[-(1:burn), ],
                 ll    =  keep.ll[-(1:burn)],
                 tau   =  keep.tau[-(1:burn)],
                 RE    =  keep.RE[-(1:burn), ],
                 acc   =  list(accRatio=acc/att,accRatio_RE=acc_RE/att_RE)
  )
  #if(mis) output$ymis =  keep.ymis[-(1:burn), ]  <- - I'll add the other missing stuff back in if there's time
  return(output)

}

#Let's try it!!!
# throwaway <- counts345[[10]]
# throwaway[c(1, 24, 33)] <- NA
#run <- N_Bmcmc(Y = counts345[[10]], N = n, X = cbind(rep(1, 38), as.numeric(aaorows), as.numeric(aaarows)), n.iters = 5000)
#run2 <- N_Bmcmc(Y = counts345[[10]], N = n, X = cbind(rep(1, 38), as.numeric(taarows), as.numeric(aaarows)), n.iters = 10000)
#run3 <- N_Bmcmc(Y = counts345[[9]], N = n, X = cbind(rep(1, 38), as.numeric(aaorows), as.numeric(aaarows)), n.iters = 10000)
#run3 <- N_Bmcmc(Y = counts345[[9]], N = n, n.iters = 10000)
run7 <- N_Bmcmc(Y = counts345[[10]], N = tab3[,5], X = cbind(rep(1, 38), as.numeric(aaorows), as.numeric(aaarows)), n.iters = 20000)


library(coda)
chain<-mcmc(run7$beta)
round(c(effectiveSize(chain)))
autocorr.plot(chain)


out.beta<-run7$beta[seq(1,nrow(run7$beta),25),]

apply(exp(out.beta),2,quantile,c(0.05,0.5,0.95))


#run7 <- N_Bmcmc(Y = counts345[[10]], N = N, n.iters = 1000)



#summary(run$beta)
#summary(exp(run$beta))

summary(run7$beta)
summary(exp(run7$beta))

#posterior distributions of missing data
#summary(run2$ymis)

