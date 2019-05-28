# testing whether the boostrap is correct for gop method
# we realize that sd from titaninc gop is quite larger than sd from closed function

rm(list = ls())

library(brm)
source("functions/generalized_op.R")
source("functions/var.R")

alpha_true = cbind(c(-0.5,1),c(0.5, 1.5))
beta_true = c(1, -0.5)

do.one = function(seed=1, sample.size, st) {
      set.seed(seed)
      v1 = rep(1, sample.size)
      v2 = runif(sample.size, -2, 2)
      va = cbind(v1, v2)
      vb = va

      nz = ncol(alpha_true) + 1

      z = sample(x = c(0:(nz-1)), size = sample.size, replace = T)
      z = as.factor(z)
      ny = sample.size

      logRR.mat = va %*% alpha_true
      logOP.mat = vb %*% beta_true

      prob.mat = matrix(NA, nrow = ny, nz)
      for(i in 1:ny){
            prob.mat[i,] = getProbScalarRR_v2(logRR.mat[i,], logOP.mat[i])

      }

      y = rep(0, sample.size)

      for(i in 1:nz){
            y[z == i-1] = rbinom(length(which(z==i-1)), 1, prob.mat[z==i-1,i])
      }

      dat = cbind(trt = z, outcome = y)
      # find the MLE
      mle = max.likelihood.v3(y, z, va, vb, alpha.start=matrix(rep(0,4),2,2), beta.start=c(0,0), max.step=500, thres=10^-4)
      # sd using close function
      sd2 = sqrt(rr.gop.var(y, z, va, vb, alpha=mle[[1]], beta = mle[[2]]))

      mle.mat = cbind(mle[[1]], mle[[2]], c(mle[[3]], mle[[4]]), mle[[5]], sd2[,-3])

      colnames(mle.mat) = c(paste("alpha", c(1: (nz-1)), sep = ""), "beta", "cnverg_logl",
                            paste("alpha", c(1: (nz-1)), "_sd", sep = ""),
                            paste("alpha", c(1: (nz-1)), "_sd2", sep = ""))

      save(mle.mat, dat, file = paste("gop-boostrap-test/data/simu_Size", sample.size, "_Seed", seed, st, ".RData", sep = ""))

}

do.one(seed = 1, sample.size = 500, st = "2019-05-27")
do.one(seed = 1, sample.size = 1000, st = "2019-05-27")
