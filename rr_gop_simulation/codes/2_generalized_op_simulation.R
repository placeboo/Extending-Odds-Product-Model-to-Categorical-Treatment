#!/usr/local/bin/Rscript
rm(list = ls())

args = commandArgs(TRUE)

sample_size = as.numeric(args[[1]])
#gamma_true = as.numeric(args[[2]])
seed = as.numeric(args[[2]])

options(error = utils::recover)
source("rr_gop_simulation/codes/1_generalized_op.R")

library(brm)
#library(tictoc)

alpha_true = cbind(c(-0.5,1),c(0.5, 1.5))
beta_true = c(1, -0.5)
#beta_true = c(1, -1)

N_sim = 200
#
# N_sim = 2
# sample_size = 100
# seed = 2

set.seed(seed)

mle.mat = replicate(N_sim, do.one.mle.sim(alpha.true = alpha_true, beta.true = beta_true, sample.size = sample_size, alpha.start = matrix(0,2,2), beta.start = c(0, 0), max.step =500, thres=10^-4))

# save simulation results
st=format(Sys.Date(), "%Y-%m-%d")
filename = paste("rr_gop_simulation/data/","SampleSize", sample_size,"_", "Seed", seed, "_", st, ".Rdata", sep = "")

save(file = filename, mle.mat)


