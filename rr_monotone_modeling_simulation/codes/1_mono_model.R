max.likelihood.v2 = function(y, z, va, vb, alpha.start, beta.start, max.step=10^5, thres=10^-5){
      if(!is.factor(z)){
            stop("primary interest should be factors!")
      }
      pa = length(alpha.start)
      pb = length(beta.start)
      ny = length(y)
      nz = nlevels(z)

      z = factor(z, levels = levels(z), labels = c(0: (nz-1)))

      neg.log.value = function(alpha, beta){
            Pzmin_Pzmax = t(mapply(getProbScalarRR, va %*% alpha * (nz-1), vb %*% beta))
            P.mat = matrix(0, ncol = nz, nrow = ny)
            P.mat[, c(1, nz)] = Pzmin_Pzmax
            P.mat[, -c(1, nz)] = Pzmin_Pzmax[,1] * exp(va %*% alpha %*% t(c(1: (nz-2))))



            n_0.vec = apply(P.mat, 2, function(x) sum(x==0))

            if(identical(all.equal(n_0.vec, rep(0, nz)), TRUE)){
                  value = 0
                  for(i in 1:nz){

                        value = value - sum(y[z==i-1] * log(P.mat[z==i-1,i]) + (1-y[z==i-1]) * log(1-P.mat[z==i-1,i]))

                  }
                  #save(z, P.mat,alpha, beta,va,Pzmin_Pzmax,nz, ny,vb, file = "test.Rdata")
                  return(value)
            }
            else{
                  return(10000)
            }
      }

      neg.log.likelihood = function(pars){
            alpha = pars[1:pa]
            beta = pars[(pa + 1):(pa + pb)]

            return(neg.log.value(alpha, beta))
      }

      neg.log.likelihood.alpha = function(alpha){
            return(neg.log.value(alpha, beta))
      }

      neg.log.likelihood.beta = function(beta){

            return(neg.log.value(alpha, beta))
      }

      Diff = function(x, y) sum((x - y)^2)/sum(x^2 + thres)
      alpha = alpha.start
      beta = beta.start
      diff = thres + 1
      step = 0


      while(diff > thres & step < max.step){
            step = step + 1
            opt1 = stats::optim(alpha, neg.log.likelihood.alpha,
                                control = list(maxit = max.step))
            diff1 = Diff(opt1$par, alpha)
            alpha = opt1$par

            opt2 = stats::optim(beta, neg.log.likelihood.beta, control = list(maxit = max.step))

            diff2 = Diff(opt2$par, beta)
            beta = opt2$par

            diff = max(diff1, diff2)
      }
      # have hessian matrix
      hessian.mat = try(optimHess(c(alpha,beta), neg.log.likelihood), silent = TRUE)
      if ("try-error" %in% class(hessian.mat)) {
            alpha.sd = rep(NA, pa)
      } else {
            alpha.sd = sqrt(diag(solve(hessian.mat)))[1: pa]
      }


      #alpha.sd = sqrt(diag(solve(hessian.mat)))[1: pa]
      #beta.hessian.mat = stats::optim(beta, neg.log.likelihood.beta, control = list(maxit = max.step), hessian = TRUE)$hessian

      opt = c(alpha, beta, step < max.step,  neg.log.likelihood(c(alpha, beta)),
              alpha.sd)

      return(opt)
}

do.one.mle.sim = function(nz, alpha.true, beta.true, sample.size, alpha.start, beta.start, max.step=10^5, thres=10^-5){

      # generate data
      v1 = rep(1, sample.size)
      v2 = runif(sample.size, -2, 2)
      va = cbind(v1, v2)
      vb = va
      z = sample(x = c(0:(nz-1)), size = sample.size, replace = T)
      z = as.factor(z)
      ny = sample.size

      Pzmin_Pzmax = t(mapply(getProbScalarRR, va %*% alpha.true * (nz-1), vb %*% beta.true))
      P.mat = matrix(0, ncol = nz, nrow = ny)
      P.mat[, c(1, nz)] = Pzmin_Pzmax
      P.mat[, -c(1, nz)] = Pzmin_Pzmax[,1] * exp(va %*% alpha.true %*% t(c(1: (nz-2))))

      y = rep(0, sample.size)

      for(i in 1:nz){
            y[z == i-1] = rbinom(length(which(z==i-1)), 1, P.mat[z==i-1,i])
      }


      # find the MLE
      mle = max.likelihood.v2(y, z, va, vb, alpha.start, beta.start)
      return(mle)
}

bias_sd = function(bias, sd){
      return(paste(round(bias, 3), "(", round(sd, 3), ")", sep = ""))
}





