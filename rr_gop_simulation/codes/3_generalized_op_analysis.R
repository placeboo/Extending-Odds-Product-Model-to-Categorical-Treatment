rm(list = ls())

library(xtable)
library(abind)
source("1_generalized_op.R")

alpha_true = cbind(c(-0.5,1),c(0.5, 1.5))
beta_true = c(1, -0.5)


sample_size.vec = c(50, 100, 200, 400, 800, 1600)
seed.vec= 1:5

# the file recording pairs without simulation results
no_sim.txt = file("no_simulation_rst.txt", "w")
conveg.txt = file("no_convergence.txt", "w")

mean_sd.mat = matrix(NA, ncol = 1, nrow = 3)
for(i in 1: length(sample_size.vec)){
        mle_tmp_dat = array(0, c(2, 4))
        for(seed in seed.vec){
                sample_size = sample_size.vec[i]
                file_name = paste("simulation_dat/sample_size_", sample_size, "seed_", seed,".Rdata", sep = "")
                load(file = file_name)
                mle_tmp_dat = abind(mle_tmp_dat, mle.mat, along = 3)
                
        }
        mle_tmp_dat = mle_tmp_dat[,,-1]
        # number of levles
        nz = dim(mle_tmp_dat)[2] - 2
        # number of covariates
        p = dim(mle_tmp_dat)[1]
        # number of simulation
        n = dim(mle_tmp_dat)[3]
        
        if(mean(mle.mat[1,4,]) != 1){
                # the number of no convergence
                index_no_convergence = which(mle_tmp_dat[1,4,] != 1)
                tmp.str = paste("the number of no convergence: ", length(index_no_convergence))
                writeLines(paste(file_name, "; ",tmp.str), conveg.txt)
                mle_tmp_dat = mle_tmp_dat[, , mle.mat[1,4, ] == 1]
                
        }
        
        # alpha
        alpha_diff.array = mle_tmp_dat[ ,c(1:nz), ] - array(alpha_true, dim = c(p,nz,n))
        alpha_bias.mat = matrix(NA, ncol = p, nrow = nz)
        alpha_sd.mat = matrix(NA, ncol = p, nrow = nz)
        for(j in 1:nz){
                alpha_bias.mat[j,] = apply(alpha_diff.array[,j,], 1, mean)
                alpha_sd.mat[j,] = apply(alpha_diff.array[,j,], 1, sd)
        }
        alpha_rst = matrix(bias_sd(alpha_bias.mat, alpha_sd.mat), nz, p)
        
        # beta
        beta_diff.mat = mle_tmp_dat[,nz+1, ] - beta_true
        beta_bias.vec = apply(beta_diff.mat, 1, mean)
        beta_sd.vec = apply(beta_diff.mat, 1, sd)
        beta_rst = bias_sd(beta_bias.vec, beta_sd.vec)
        
        mean_sd_tmp.mat = rbind(alpha_rst, beta_rst)
        
        mean_sd_tmp_colname = paste("n=", sample_size, sep = "")
        colnames(mean_sd_tmp.mat) = c(mean_sd_tmp_colname,mean_sd_tmp_colname)
        mean_sd.mat = cbind(mean_sd.mat, mean_sd_tmp.mat)
}

 
rownames(mean_sd.mat) = c(paste("alpha", 1:nz, sep = ""), "beta")
mean_sd.mat = mean_sd.mat[,-1]
write.csv(mean_sd.mat, file = paste("mle_rst.csv", sep = ""))
# save the text table
tmp.tex = xtable(t(as.matrix(mean_sd.mat)), caption = "bias and its sd")
print(tmp.tex, file = paste("simulaton_result/general_op_simulation.tex", sep = ""))

#sink()
close(conveg.txt)
close(no_sim.txt)

