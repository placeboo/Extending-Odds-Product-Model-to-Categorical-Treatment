same = function (x, y, tolerance = .Machine$double.eps^0.5) {
        abs(x - y) < tolerance
}



getProbScalarRR_v2 = function(logrr, logop){
        k = length(logrr) # levels

        logrr_max = max(c(0,logrr))

        # logrr_ = c(1, exp(logrr))
        # rr_max = max(rr)

        rr_update =exp(c(0, logrr) - logrr_max)

        f = function(x){
                return((k+1) * log(x) + sum(log(rr_update)) - sum(log(1 - x * rr_update)) - logop)
        }

        tol = 10
        if(f(1-0.1^tol)<0) p.max = 1 else{
                p.max = uniroot(f, lower=0, upper=1, tol=10^(-tol))$root
        }
        return(p.max * rr_update)
}

max.likelihood.v3 = function(y, z, va, vb, alpha.start, beta.start, max.step=1000, thres=10^-5){
        if(is.vector(alpha.start)){# imply the primary of interest having two levels
                alpha.start = as.matrix(alpha.start)
        }
        pa = nrow(alpha.start)
        num_rr = ncol(alpha.start) # how many models for log(RR)

        pb = length(beta.start)
        ny = length(y)
        nz = nlevels(z)

        z = factor(z, levels = levels(z), labels = c(0: (nz-1)))

        neg.log.value = function(alpha, beta){

                logRR.mat = va %*% alpha
                logOP.mat = vb %*% beta

                prob.mat = matrix(NA, nrow = ny, nz)
                for(i in 1:ny){
                        prob.mat[i,] = getProbScalarRR_v2(logRR.mat[i,], logOP.mat[i])
                        #print(i)
                }
                n_0.vec = apply(prob.mat, 2, function(x) sum(x==0))
                if(identical(all.equal(n_0.vec, rep(0, nz)), TRUE)){
                        value = 0
                        for(i in 1:nz){
                                value = value - sum(y[z==i-1] * log(prob.mat[z==i-1,i]) + (1-y[z==i-1]) * log(1-prob.mat[z==i-1,i]))
                        }
                        return(value)
                }else{
                        return(10000)
                }
        }

        neg.log.likelihood = function(pars){
                alpha = pars[[1]]
                beta = pars[[2]]

                return(neg.log.value(alpha, beta))
        }
        neg.log.likelihood.alpha = function(alphai){
                alpha[,alpha_index] = alphai
                return(neg.log.value(alpha, beta))
        }
        neg.log.likelihood.beta = function(beta){
                return(neg.log.value(alpha, beta))
        }
        Diff = function(x, y){
                return(sum((x - y)^2)/sum(x^2 + thres))
        }
        alpha = alpha.start
        beta = beta.start
        diff = thres + 1
        step = 0

        while(diff > thres & step < max.step){
                step = step + 1
                # go through each log(RR) model
                diff_alpha = rep(NA, num_rr)
                for(alpha_index in c(1: num_rr)){
                        alphai = alpha[, alpha_index]
                        opti = stats::optim(alphai, neg.log.likelihood.alpha,
                                            control = list(maxit = max.step))
                        alpha[,alpha_index] = opti$par # update
                        diff_alpha[alpha_index] = Diff(opti$par, alphai)
                }
                opt2 = stats::optim(beta, neg.log.likelihood.beta, control = list(maxit = max.step))

                diff2 = Diff(opt2$par, beta)
                beta = opt2$par

                diff = max(max(diff_alpha), diff2)
        }
        # have sd for alpha
        sd.mat = matrix(NA, ncol = num_rr, nrow = pa)
        for (alpha_index in c(1: num_rr)){
                alphai = alpha[, alpha_index]
                opti = try(stats::optim(alphai, neg.log.likelihood.alpha,
                                    control = list(maxit = max.step), hessian = TRUE), silent  = TRUE)
                if ("try-error" %in% class(opti)) { # there is no hession (non finite)
                        next
                }
                alpha[,alpha_index] = opti$par # update
                sd.mat[,alpha_index] = sqrt(diag(solve(opti$hessian)))
        }

        opt = list(alpha, beta, step < max.step,  neg.log.likelihood(list(alpha, beta)), sd.mat)

        return(opt)
}

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
        mle.mat = cbind(mle[[1]], mle[[2]], c(mle[[3]], mle[[4]]), mle[[5]])

        colnames(mle.mat) = c(paste("alpha", c(1: (nz-1)), sep = ""), "beta", "cnverg_logl",
                              paste("alpha", c(1: (nz-1)), "_sd", sep = ""))
        return(mle.mat)
}

bias_sd = function(bias, sd){
        return(paste(round(bias, 3), "(", round(sd, 3), ")", sep = ""))
}


