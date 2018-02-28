library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model_code_a = "
data {
int<lower=0> K; // number of studies
int<lower=1> O[K]; // offset term - number of ankles
int<lower=0> NY_rep; // num studies reporting total complications
int<lower=0> NY_mis; // num studies missing total complications
int<lower=1, upper=NY_rep+NY_mis> ii_rep[NY_rep]; // indeces of reported Y
int<lower=1, upper=NY_rep+NY_mis> ii_mis[NY_mis]; // indeces of missing Y
real<lower=0> Y_rep[NY_rep]; // values of observed Ys
}
parameters {
vector<lower=0>[NY_mis] Y_mis; //the values of the missing Ys 
vector<lower=0>[K] lambda; // mean & variance of poisson model
}
transformed parameters {
vector<lower=0>[K]Y; //ALL of standard deviations
Y[ii_rep]=Y_rep; //putting the reported Ys into Y
Y[ii_mis]=Y_mis; //putting the missing Ys into Y
vector<lower=0,upper=1>[K] R=Y/O;
}
model {
//N~poisson(lambda);
target += poisson_log_lpmf(R|lambda);
lambda~beta(1,1)
}
"

sc.a<-stanc(model_code=model_code_a)

m.a <- stan_model(model_code=model_code_a)
alldat<-list(K=length(n),O=total, NY_rep=sum(!is.na(comp.ovr)),N_mis=sum(is.na(comp.ovr)),ii_obs=which(!is.na(comp.ovr)),
             ii_mis=which(is.na(comp.ovr)),LN_report=comp.ovr[!is.na(comp.ovr)])
modcod.a.samps <- sampling(m.a,data=alldat,iter=1000,chains=3)

print(modcod.a.samps)
stan_hist(modcod.a.samps, pars="R")
stan_ac(modcod.a.samps,pars="R")
