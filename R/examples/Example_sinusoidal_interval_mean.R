library(PriorRhythm)

# Scalar usage — single time bin, single amplitude
sinusoidal_interval_mean(A = 5, t1 = 0, t2 = 3, phi = 0)

# Vectorised over MCMC samples (as used internally)
set.seed(42)
n_samp <- 100
A_samples   <- rnorm(n_samp, mean = 5,  sd = 1)
phi_samples <- runif(n_samp, min = 0,   max = 2 * pi)

mean_0_3  <- sinusoidal_interval_mean(A = A_samples, t1 = 0,  t2 = 3,  phi = phi_samples)
mean_3_6  <- sinusoidal_interval_mean(A = A_samples, t1 = 3,  t2 = 6,  phi = phi_samples)
mean_6_12 <- sinusoidal_interval_mean(A = A_samples, t1 = 6,  t2 = 12, phi = phi_samples)

# Summarise
summary(mean_0_3)
