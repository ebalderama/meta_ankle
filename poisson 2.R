library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

model_code_c = "
data {
int<lower=0> K; // number of studies
real<lower=1> O[K]; // offset term - number of ankles
real<lower=0> Y[K]; // failures
}
transformed data {
vector<lower=0,upper=1>[K] R=Y[K]/O[K];
}
parameters {
vector<lower=0>[K] lambda; // mean & variance of poisson model
}
model {
//N~poisson(lambda);
target += poisson_log_lpmf(R|lambda);
lambda~beta(1,1)
}
"

sc.c<-stanc(model_code=model_code_c)

m.c <- stan_model(model_code=model_code_c)
alldat<-list(K=length(n),O=total, Y=failx)
modcod.c.samps <- sampling(m.c,data=alldat,iter=1000,chains=3)

print(modcod.c.samps)
stan_hist(modcod.c.samps, pars="R")
stan_ac(modcod.c.samps,pars="R")
