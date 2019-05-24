rm(list = ls())

load(file = "titanic/data/data_remove_missing.RData")
st=format(Sys.Date(), "%Y-%m-%d")
coefs=c("pclass2nd","pclass2nd:sexmale","pclass2nd:age","pclass2nd:age_sq", "pclass2nd:sexmale:age",
        "pclass3rd","pclass3rd:sexmale","pclass3rd:age","pclass3rd:age_sq", "pclass3rd:sexmale:age")

n = nrow(dat_nm)
N_sim = 1000

do.one.poisson = function(){
      idex = sample(1:n, n, replace = TRUE)

      dat_idex = dat_nm[idex, ]


      # there is incovergence
      poi_coef=tryCatch(coef(glm(dead ~ pclass + sex + age + age_sq + age * sex + pclass * sex + pclass * age + pclass * age_sq +  pclass * age * sex , family = poisson, data = dat_idex))[coefs],
                    warning = function(w) return(rep(NA, 10))
      )

      #poi_coef=coef(glm(dead ~ pclass + sex + age + age_sq + age * sex + pclass * sex + pclass * age + pclass * age_sq +  pclass * age * sex , family = poisson, data = dat_idex))[coefs]

    return(poi_coef)


     # (converted from warning) glm.fit: fitted rates numerically 0 occur
}

set.seed(1)
poi.bootst.mat = replicate(N_sim, do.one.poisson())



filename = paste("titanic/data/poisson_bootstrap_Seed",1, "_", st, ".RData", sep = "")
save(file = filename, poi.bootst.mat)


