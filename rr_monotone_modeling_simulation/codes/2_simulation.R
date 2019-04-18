#!/usr/local/bin/Rscript
rm(list = ls())

args = commandArgs(TRUE)

sample_size = as.numeric(args[[1]])
seed = as.numeric(args[[2]])

options(error = utils::recover)
source("rr_monotone_modeling_simulation/codes/1_mono_model.R")

library(brm)

alpha_true = c(0, 1)
beta_true = c(-0.5, 1)
st=format(Sys.Date(), "%Y-%m-%d")

N_sim = 200
for (sample_size in c(50, 100, 500, 1000, 5000, 10000)){
      print(paste("sample size:", sample_size))
      for (seed in c(1:5)){
            print(paste("seed=",seed))
            set.seed(seed)
            mle.mat = replicate(N_sim, do.one.mle.sim(nz = 3, alpha.true = alpha_true, beta.true = beta_true, sample.size = sample_size, alpha.start = c(0, 0), beta.start = c(0, 0), max.step = 500, thres=10^-4))

            rownames(mle.mat) = c("alpha0", "alpha1", "beta0", "beta1", "convergence", "likelihood", "alpha0_sd", "alpha1_sd")

            # save simulation results
            filename = paste("rr_monotone_modeling_simulation/data2/","SampleSize", sample_size,"_", "Seed", seed, "_", st, ".Rdata", sep = "")

            save(file = filename, mle.mat)
      }


}


