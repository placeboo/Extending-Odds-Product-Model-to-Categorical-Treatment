rm(list = ls())
source("rr_monotone_modeling_simulation/codes/1_mono_model.R")

sample_size_vector = c(50, 100, 500, 1000, 5000)
date = "2019-02-18"
seed = 14
alpha_true = c(0, 1)
beta_true = c(-0.5, 1)
N_sim = 1000

bias.mat = matrix(NA, nrow = length(sample_size_vector), ncol = 2)
accuracy.mat = matrix(NA, nrow = length(sample_size_vector), ncol = 2)
cr.mat = matrix(NA, nrow = length(sample_size_vector), ncol = 2)

for(i in 1:length(sample_size_vector)){
      size_i = sample_size_vector[i]
      filename = paste("rr_monotone_modeling_simulation/data/SampleSize", size_i, "_Seed", seed, "_", date, ".Rdata", sep = "")
      load(filename)

      # bias of the estimation
      bias = apply(mle.mat[c("alpha0","alpha1"),]- c(alpha_true),1, mean)
      # se of bias
      se = apply(mle.mat[c("alpha0","alpha1"),]- c(alpha_true),1, sd)
      bias.mat[i, ] = bias_sd(bias, se)

      # Monte Carlo SD
      alpha_mcsd = apply(mle.mat[c("alpha0","alpha1"),], 1, sd)
      # estimated standard deviation
      alpha_sd = apply(mle.mat[c("alpha0_sd", "alpha1_sd"),], 1, mean)
      accuracy.mat[i,] = round(alpha_sd / alpha_mcsd, 3)
      # covarage rate

      lowerB = mle.mat[c("alpha0","alpha1"),] - 1.96 * mle.mat[c("alpha0_sd", "alpha1_sd"),]
      upperB = mle.mat[c("alpha0","alpha1"),] +  1.96 * mle.mat[c("alpha0_sd", "alpha1_sd"),]

      ## alph0
      alpha0_cr = round(sum(alpha_true[1] >lowerB[1, ] & alpha_true[1] < upperB[1, ]) / N_sim, 3)
      ## alpha1
      alpha1_cr = round(sum(alpha_true[2] > lowerB[2, ] & alpha_true[2] < upperB[2, ]) / N_sim, 3)
      cr.mat[i, ] = c(alpha0_cr, alpha1_cr)
}


filename = paste("rr_monotone_modeling_simulation/data/anlysis_Seed", seed, "_", date,".RData", sep = "")
save(bias.mat, accuracy.mat, cr.mat, file = filename)


# latex output
library(xtable)
table = rbind(bias.mat, accuracy.mat, cr.mat)
colnames(table) = c("alpha_0", "alpha_1")
rownames(table) = rep(sample_size_vector, 3)
print(xtable(table))



