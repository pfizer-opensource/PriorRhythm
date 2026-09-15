# chain_model.R ----
#
# Hierarchical sinusoidal + dose-response JAGS model (Chain et al., 2013)
#
# .chain_model_jags is the JAGS model string for the hierarchical sinusoidal
# + dose-response model used by chain_priors_from_data_helper() via rjags.
#
# Reference: Chain, A.S.Y. et al. (2013). Identifying the translational gap in
# the evaluation of drug-induced QTc interval prolongation.
# British Journal of Clinical Pharmacology, 76(5).
#
# Changes from original parameterisation (Dean Li, 2025):
#   - phi is now a single shared scalar phi0 ~ dunif(0, 2*pi) rather than a
#     per-study random effect.  The mu formula uses phi0 directly.
#   - sig_v is now a single shared scalar rather than a per-period vector.
#   - alpha0 prior uses alpha0_pop_mean (passed as JAGS data, default 0) with
#     precision 0.0004 (was dnorm(0, 0.001)).
#
# Note: JAGS does not have access to R's `pi` constant, so 3.14159 is used
# directly in the model string. The R-side helper sinusoidal_interval_mean()
# uses R's full-precision `pi` and must remain mathematically consistent with
# this string.
.chain_model_jags <- "
model{
for(i in 1:N){
y[i] ~ dnorm( mu[i],   pow(sig[s[i]], -2) )
mu[i] <-  alpha[s[i]]+
A[s[i]]*(12/3.14159/(time2[i]-time1[i]))*(sin(3.14159/12*time2[i]+phi0)-sin(3.14159/12*time1[i]+phi0))+
beta[s[i]]*dose[i]  +
u[a[i]] + v[b[i]]
}

for(i in 1:na){
u[i] ~ dnorm(0, pow(sig_u[sa[i]], -2) )
}

for(i in 1:nb){
v[i] ~ dnorm(0, pow(sig_v, -2) )
}

for(j in 1:ns){
alpha[j] ~ dnorm(alpha0, pow(sig_alpha, -2) )
A[j] ~ dnorm(A0, pow(sig_A, -2) )
beta[j] ~ dnorm(beta0, pow(sig_beta, -2) )
sig[j] ~ dt(0, pow(2.5,-2), 1) T(0,)
sig_u[j] ~ dt(0, pow(2.5,-2), 1) T(0,)
}

sig_v ~ dt(0, pow(2.5,-2), 1) T(0,)

alpha0 ~ dnorm(alpha0_pop_mean, 0.0004) T(0,)
A0 ~ dnorm(0, 0.001) T(0,)
beta0 ~ dnorm(0, 0.001)
phi0 ~ dunif(0, 6.28318)
sig_A ~ dt(0, pow(2.5,-2), 1) T(0,)
sig_alpha ~ dt(0, pow(2.5,-2), 1) T(0,)
sig_beta ~ dt(0, pow(2.5,-2), 1) T(0,)
}
"
