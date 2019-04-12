# in 2_generalized_op_simulation, for small sample size (n=100),
# there is report for the sd when uisng optimHess.
# this file for n=100, have sd use close function for Hessian matrix
rm(list = ls())

args = commandArgs(TRUE)
seed = as.numeric(args[[1]])

library(brm)
source("functions/generalized_op.R")
source("functions/var2.R")


alpha_true = cbind(c(-0.5,1),c(0.5, 1.5))
beta_true = c(1, -0.5)
sample_size = 100
st=format(Sys.Date(), "%Y-%m-%d")
#beta_true = c(1, -1)

do.one.mle.sim = function(alpha.true, beta.true, sample.size, alpha.start, beta.start, max.step=10^5, thres=10^-5){
      # generate data
      #print(max.step)
      v1 = rep(1, sample.size)
      v2 = runif(sample.size, -2, 2)
      va = cbind(v1, v2)
      vb = va

      nz = ncol(alpha.start) + 1

      z = sample(x = c(0:(nz-1)), size = sample.size, replace = T)
      z = as.factor(z)
      ny = sample.size

      logRR.mat = va %*% alpha.true
      logOP.mat = vb %*% beta.true

      prob.mat = matrix(NA, nrow = ny, nz)
      for(i in 1:ny){
            prob.mat[i,] = getProbScalarRR_v2(logRR.mat[i,], logOP.mat[i])

      }

      y = rep(0, sample.size)

      for(i in 1:nz){
            y[z == i-1] = rbinom(length(which(z==i-1)), 1, prob.mat[z==i-1,i])
      }

      # find the MLE
      mle = max.likelihood.v3(y, z, va, vb, alpha.start, beta.start, max.step, thres)
      # sd using close function
      sd2 = sqrt(rr.gop.var(y, z, va, vb, alpha=mle[[1]], beta = mle[[2]]))

      mle.mat = cbind(mle[[1]], mle[[2]], c(mle[[3]], mle[[4]]), mle[[5]], sd2[,-3])

      colnames(mle.mat) = c(paste("alpha", c(1: (nz-1)), sep = ""), "beta", "cnverg_logl",
                            paste("alpha", c(1: (nz-1)), "_sd", sep = ""),
                            paste("alpha", c(1: (nz-1)), "_sd2", sep = ""))
      return(mle.mat)
}


N_sim = 100
set.seed(seed)

mle.mat = replicate(N_sim, do.one.mle.sim(alpha.true = alpha_true, beta.true = beta_true, sample.size = sample_size, alpha.start = matrix(0,2,2), beta.start = c(0, 0), max.step =500, thres=10^-4))


# save simulation results
filename = paste("rr_gop_simulation/data4/","SampleSize", sample_size,"_", "Seed", seed, "_", st, ".Rdata", sep = "")

save(file = filename, mle.mat)



