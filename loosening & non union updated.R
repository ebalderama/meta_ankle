library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Missing data for loosening/nonunion (henceforth referred to as L.N)
model_code_b = "
data {
int<lower=0> K; //number of studies
int<lower=0> N[K]; //number of ankles per each study
int<lower=0> N_rep; //number of studies with reported L.N
int<lower=0> N_mis; //number of studies not reporting L.N
int<lower=1, upper=N_rep+N_mis> ii_obs[N_rep]; //indeces for reported L.Ns
int<lower=1, upper=N_rep+N_mis> ii_mis[N_mis]; //indeces for missing L.Ns
int<lower=0> LN_rep[N_rep]; //The counts for reported L.Ns
}
transformed data {

}
parameters {
vector<lower=0>[N_mis] LN_mis; //the counts of the missing L.Ns
vector<lower=0,upper=1>[K] p;
}
transformed parameters {
vector<lower=0>[K] LN; //ALL of L.N counts
LN[ii_obs]=LN_rep; //putting the observed L.N counts into LN
LN[ii_mis]=LN_mis; //putting the estimated/missing L.N counts into LN
vector<lower=0,upper=1>[K] p=LN/N; //probability of an ankle having LN
}
model {
target+=binomial_lpdf(LN|N,p); //binomial for loosening/nonunion
p~beta(1,1); //prior for p. This ain't gonna work, probably
}
"

sc.b<-stanc(model_code=model_code_b)

m.b <- stan_model(model_code=model_code_b)

alldat<-list(K=length(n), N=n, N_rep=sum(!is.na(loose_nonun)),N_mis=sum(is.na(loose_nonun)),ii_obs=which(!is.na(loose_nonun)),
             ii_mis=which(is.na(loose_nonun)),LN_rep=loose_nonun[!is.na(loose_nonun)])

modcod.a.samps <- sampling(m.a,data=alldat,iter=1000,chains=3)

print(modcod.b.samps)

stan_hist(modcod.b.samps, pars="LN")