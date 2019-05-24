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
N_sim = 5

do.one.gop = function(){
      idex = sample(1:n, n, replace = TRUE)

      dat_idex = dat_nm[idex, ]
      v1_idex = v1[idex, ]

      gop.md = max.likelihood.v3(y=dat_idex$dead, z = as.factor(dat_idex$pclass_num), va = v1_idex, vb = v1_idex, alpha.start = matrix(0, 5, 2), beta.start = rep(0,5))

      gop_coef  = as.vector(gop.md[[1]])

      return(gop_coef)
}

set.seed(seed)
gop.bootst.mat = replicate(N_sim, do.one.gop())

filename = paste("titanic/data/gop_bootstrap_Seed",seed, "_", st, ".RData", sep = "")
save(file = filename, gop.bootst.mat)
