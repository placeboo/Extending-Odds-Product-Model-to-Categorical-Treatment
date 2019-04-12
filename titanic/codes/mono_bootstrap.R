#!/usr/local/bin/Rscript
rm(list = ls())

args = commandArgs(TRUE)
seed = as.numeric(args[[1]])

source("functions/generalized_op.R")
source("functions/mono_model.R")
load(file = "titanic/data/v1.RData")
load(file = "titanic/data/data_remove_missing.RData")
st=format(Sys.Date(), "%Y-%m-%d")

n = dim(v1)[1]
N_sim = 100

do.one.mono = function(){
      idex = sample(1:n, n, replace = TRUE)

      dat_idex = dat_nm[idex, ]
      v1_idex = v1[idex, ]

      mle_mono.md = max.likelihood.v2(y=dat_idex$dead, z = as.factor(dat_idex$pclass_num), va = v1_idex, vb = v1_idex, alpha.start = c(0,0,0,0,0), beta.start = c(0,0,0,0,0))

      return(mle_mono.md[1:5])
}

set.seed(seed)
mono.mat = replicate(N_sim, do.one.mono())

filename = paste("titanic/data/mono_bootstrap_Seed",seed, "_", st, ".RData", sep = "")
save(file = filename, mono.mat)
