library(brm)
source(file = "functions/generalized_op.R")


same = function (x, y, tolerance = .Machine$double.eps^0.5) {
      abs(x - y) < tolerance
}

rr.mono.var = function(y, tr, va, vb, gamma, beta){
      # y: outcome, binary numeric
      # tr: treatment, factors
      # x: covariates, matrix, each entry is numeric
      # gamma: coefs for rr, numeric
      # beta: coefs for op, numerc
      # return: variance matrix for (gamma, beta)
      if (!is.numeric(y)) {
            stop("outcome should be numeric!")
      }
      if (!is.factor(tr)) {
            stop("treatment should be factor!")
      }

      nz = nlevels(tr)

      ny = length(y)

      z = factor(tr, levels = levels(tr), labels = c(0:(nz-1)))
      z.num = as.numeric(z)

      Pzmin_Pzmax = t(mapply(getProbScalarRR, va %*% gamma * (nz-1), vb %*% beta))
      P.mat = matrix(0, ncol = nz, nrow = ny)
      P.mat[, c(1, nz)] = Pzmin_Pzmax
      P.mat[, -c(1, nz)] = Pzmin_Pzmax[,1] * exp(va %*% gamma %*% t(c(1: (nz-2))))

      pi.vec = rep(NA, ny)
      for (j in 1:ny){
            pi.vec[j] = P.mat[j, z.num[j]]
      }

      pl_ppsi = (y - pi.vec) / (1 - pi.vec)

      ppsi_ptheta = - (nz-1) * (1 - P.mat[,1]) / (1 - P.mat[,1] + 1 - P.mat[, nz])

      ppsi_pphi = (1 - P.mat[, nz]) *  (1 - P.mat[, 1]) /(1 - P.mat[, nz] +  1 - P.mat[, 1])

      pl_pgamma = pl_ppsi * ((z.num-1) + ppsi_ptheta) * va
      pl_pbeta = pl_ppsi * ppsi_pphi * vb

      tmp1 = cbind(t(pl_pgamma) %*% pl_pgamma, t(pl_pgamma)  %*% pl_pbeta)
      tmp2 = cbind(t(pl_pbeta) %*% pl_pgamma, t(pl_pbeta) %*% pl_pbeta)

      hessian.mat = rbind(tmp1, tmp2)

      rownames(hessian.mat) = NULL
      colnames(hessian.mat) = NULL
      var.mat = solve(hessian.mat)

      return(diag(var.mat))
}


rr.gop.var = function(y, tr, va, vb, alpha, beta){
      # y: outcome, binary numeric
      # tr: treatment, factors
      # x: covariates, matrix, each entry is numeric
      # alpha: coefs for rrs, numeric
      # beta: coefs for gop, numerc
      # return: variance matrix for (alpha, beta)

      if (!is.numeric(y)) {
            stop("outcome should be numeric!")
      }
      if (!is.factor(tr)) {
            stop("treatment should be factor!")
      }

      nz = nlevels(tr)
      ny = length(y)

      z = factor(tr, levels = levels(tr), labels = c(0: (nz-1)))
      z.num = as.numeric(z)

      logRR.mat = va %*% alpha
      logOP.mat = vb %*% beta

      P.mat = matrix(NA, nrow = ny, nz)
      for(i in 1:ny){
            P.mat[i,] = getProbScalarRR_v2(logRR.mat[i,], logOP.mat[i])
      }

      # if there is 1 in P.mat, modify it.
      P.mat[same(P.mat,1)] = 0.9999

      pi.vec = rep(NA, ny)
      for (j in 1:ny){
            pi.vec[j] = P.mat[j, z.num[j]]
      }

      pl_alpha = matrix(NA, nrow = nrow(va))

      tmp1 = (y - pi.vec) / (1 - pi.vec)
      tmp2 = 1 / apply(P.mat, 1, function(x) sum(1/(1-x)))

      for (j in 1:(nz-1)) {
            tmp3 = 1 / (1 - P.mat[,j+1])

            pl_palpha_j =  tmp1 * ((z == j) - tmp3 * tmp2 ) * va
            pl_alpha = cbind(pl_alpha, pl_palpha_j)
      }

      pl_alpha = pl_alpha[,-1]

      # l to beta

      pl_pbeta = tmp1 * tmp2 * vb

      pl_p = cbind(pl_alpha, pl_pbeta)

      hessian.mat = t(pl_p) %*% pl_p

      rownames(hessian.mat) = NULL
      colnames(hessian.mat) = NULL
      cov.mat = solve(hessian.mat)

      var.vec = diag(cov.mat)

      var.mat = matrix(var.vec, nrow = ncol(va), ncol = nz, byrow = FALSE)
      return(var.mat)
}

