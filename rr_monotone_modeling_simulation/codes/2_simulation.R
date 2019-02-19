#!/usr/local/bin/Rscript
rm(list = ls())

args = commandArgs(TRUE)

sample_size = as.numeric(args[[1]])
seed = as.numeric(args[[2]])

options(error = utils::recover)
source("codes/1_mono_model.R")

library(brm)

alpha_true = c(0, 1)
beta_true = c(-0.5, 1)

N_sim = 1000

set.seed(seed)
mle.mat = replicate(N_sim, do.one.mle.sim(nz = 3, alpha.true = alpha_true, beta.true = beta_true, sample.size = sample_size, alpha.start = c(0, 0), beta.start = c(0, 0), max.step = 1000, thres=10^-5))

rownames(mle.mat) = c("alpha0", "alpha1", "beta0", "beta1", "convergence", "likelihood", "alpha0_sd", "alpha1_sd")

# save simulation results
st=format(Sys.Date(), "%Y-%m-%d")
filename = paste("rr_monotone_modeling_simulation/data/","SampleSize", sample_size,"_", "Seed", seed, "_", st, ".Rdata", sep = "")

save(file = filename, mle.mat)


