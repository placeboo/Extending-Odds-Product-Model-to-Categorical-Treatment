rm(list = ls())

library(xtable)
library(abind)
source("rr_gop_simulation/codes/1_generalized_op.R")

alpha_true = cbind(c(-0.5,1),c(0.5, 1.5))
beta_true = c(1, -0.5)
date = "2019-03-01"

sample_size.vec = c(100, 500, 1000)
seed.vec= 1:10

# the file recording pairs without simulation results
# no_sim.txt = file("no_simulation_rst.txt", "w")
# conveg.txt = file("no_convergence.txt", "w")

mean_sd.mat = matrix(NA, ncol = 1, nrow = 3)

bias.mat = matrix(NA,  nrow = 2)
accuracy.mat = matrix(NA,  nrow = 2)
cr.mat = matrix(NA,  nrow = 2)
na.count = matrix(NA, nrow = 2)
for(i in 1: length(sample_size.vec)){
        mle_tmp_dat = array(0, c(2, 6))
        for(seed in seed.vec){
                sample_size = sample_size.vec[i]
                file_name = paste("rr_gop_simulation/data3/SampleSize", sample_size, "_Seed", seed,"_", date,".Rdata", sep = "")
                load(file = file_name)
                mle_tmp_dat = abind(mle_tmp_dat, mle.mat, along = 3)

        }
        mle_tmp_dat = mle_tmp_dat[,,-1]
        # number of levles
        nz = 2
        # number of covariates
        p = dim(mle_tmp_dat)[1]
        # number of simulation
        n = dim(mle_tmp_dat)[3]

        # if(mean(mle_tmp_dat[1,4,]) != 1){
        #         # the number of no convergence
        #         index_no_convergence = which(mle_tmp_dat[1,4,] != 1)
        #         tmp.str = paste("the number of no convergence: ", length(index_no_convergence))
        #         writeLines(paste(file_name, "; ",tmp.str), conveg.txt)
        #         mle_tmp_dat = mle_tmp_dat[, , mle_tmp_dat[1,4, ] == 1]
        #
        # }

        # estimated sd appear NA
        na_count_tmp = apply(mle_tmp_dat[,c("alpha1_sd", "alpha2_sd"), ], 2, function(x) sum(is.na(x)))/2/1000
        na.count = cbind(na.count, na_count_tmp)

        # alpha
        # # bias of the estimation
        alpha_diff.array = mle_tmp_dat[ ,c(1:2), ] - array(alpha_true, dim = c(p,nz,n))

        bias = apply(alpha_diff.array, c(1,2), mean)
        se = apply(alpha_diff.array, c(1,2), sd)/sqrt(sample_size)



        bias_se = matrix(bias_sd(bias, se), nz, p)
        bias.mat = cbind(bias.mat, bias_se)


        # Monte Carlo SD
        alpha_mcsd = apply(mle_tmp_dat[ ,c(1:2), ], c(1,2), sd)

        # estimated standard deviation
        ## note that there are NA for some runs
        sd_tmp = apply(mle_tmp_dat[,c("alpha1_sd","alpha2_sd"),], c(1,2), function(x) mean(x, na.rm = TRUE))
        accuracy.mat = cbind(accuracy.mat, round(sd_tmp / alpha_mcsd, 3))

        # covarage rate
        lowerB = mle_tmp_dat[,c("alpha1","alpha2"),] - 1.96 * mle_tmp_dat[,c("alpha1_sd", "alpha2_sd"),]
        upperB = mle_tmp_dat[,c("alpha1","alpha2"),] + 1.96 * mle_tmp_dat[,c("alpha1_sd", "alpha2_sd"),]

        # odds index in alpha_11
        alpha1_cr = c(sum((alpha_true[,1] > lowerB[,1,] & alpha_true[,1] < upperB[, 1, ])[seq(1, 2000, 2)]),
                      sum((alpha_true[,1] > lowerB[,1,] & alpha_true[,1] < upperB[, 1, ])[seq(2, 2000, 2)]))/1000

        alpha2_cr = c(sum((alpha_true[,2] > lowerB[,2,] & alpha_true[,2] < upperB[, 2, ])[seq(1, 2000, 2)]),
                      sum((alpha_true[,2] > lowerB[,2,] & alpha_true[,2] < upperB[, 2, ])[seq(2, 2000, 2)]))/1000
        cr.mat = cbind(cr.mat, cbind(alpha1_cr, alpha2_cr))
}
na.count = na.count[, -1]
bias.mat = bias.mat[, -1]
accuracy.mat = accuracy.mat[, -1]
cr.mat = cr.mat[, -1]


colnames(na.count) = sample_size.vec
colnames(bias.mat) = rep(sample_size.vec, each = 2)
colnames(accuracy.mat) = rep(sample_size.vec, each = 2)
colnames(cr.mat) = rep(sample_size.vec, each = 2)

# rownames(mean_sd.mat) = c(paste("alpha", 1:nz, sep = ""), "beta")
# mean_sd.mat = mean_sd.mat[,-1]
# write.csv(mean_sd.mat, file = paste("mle_rst.csv", sep = ""))
# # save the text table
# tmp.tex = xtable(t(as.matrix(mean_sd.mat)), caption = "bias and its sd")
# print(tmp.tex, file = paste("simulaton_result/general_op_simulation.tex", sep = ""))
#
# #sink()
# close(conveg.txt)
# close(no_sim.txt)
table = rbind(bias.mat, accuracy.mat, cr.mat)
print(xtable(table, digits = 3))
