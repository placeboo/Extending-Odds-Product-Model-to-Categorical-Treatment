rm(list = ls())

args = commandArgs(TRUE)
sample_size = as.numeric(args[[1]])
seed = as.numeric(args[[2]])

N_sim = 50
library(brm)
source("functions/generalized_op.R")
source("functions/var.R")
load(paste("gop-bootstrap-test/data/simu_Size", sample_size, "_Seed1_2019-05-27.RData",sep = ""))

alpha_true = cbind(c(-0.5,1),c(0.5, 1.5))
beta_true = c(1, -0.5)

do.one.gop = function(){
      idex = sample(1:sample_size, sample_size, replace = TRUE)

      dat_idex = dat[idex, ]
      v1_idex = va[idex, ]

      gop.md = max.likelihood.v3(y=dat_idex[,2], z = as.factor(dat_idex[,1]), va = v1_idex, vb = v1_idex, alpha.start = matrix(0, 2, 2), beta.start = rep(0,2))

      gop_coef  = c(as.vector(gop.md[[1]]), as.vector(gop.md[[2]]), gop.md[[3]]) # c(alpha, beta, covergence)

      return(gop_coef)
}

set.seed(seed)
gop.bootst.mat = replicate(N_sim, do.one.gop())

filename = paste("gop-boostrap-test/data/gop_bootstrap_Size", sample_size, "Seed",seed, "_", st, ".RData", sep = "")

save(file = filename, gop.bootst.mat)
