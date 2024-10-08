

# Dependecies:
# Matrix 1.5.3
# MatrixModels 0.5-3
# TMB 1.9.14
# RTMB 1.5
library(RTMB)



linf = 500
vbk = 0.2
t0 = -1.5
sig = 30
age_i = rep(0:35, 5)
n_fish = length(age_i)

set.seed(123)
l_obs_i = rnorm(n_fish, mean = linf * (1 - exp(-vbk * (age_i - t0))), sd = sig)


pars = list(
  log_linf = log(500),
  log_vbk = log(0.2),
  t0 = -1.5,
  log_sd = log(20)
)

data = list(
  n = n_fish,
  age = age_i,
  l_obs_i = l_obs_i
)




f = function(pars) {
  getAll(data, pars)
  linf = exp(log_linf)
  vbk = exp(log_vbk)
  sd = exp(log_sd)
  l_pred = linf * (1 - exp(-vbk * (age_i - t0)))
  nll = -sum(dnorm(l_obs_i, l_pred, sd, TRUE))
  REPORT(linf)
  REPORT(vbk)
  ADREPORT(sd)
  nll
}

obj = MakeADFun(f, pars)
opt = nlminb(obj$par, obj$fn, obj$gr)
opt

sdr = sdreport(obj)
sdr

sdr$value
obj$report(opt$par)
