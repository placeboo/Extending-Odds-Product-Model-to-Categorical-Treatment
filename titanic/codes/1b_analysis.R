# In this file, we remove age^2
rm(list = ls())

library(ggplot2)
library(dplyr)
library(brm)
library(tidyr)
library(xtable)
library(rms)
library(sandwich)
## ----load functions-----------------------------------------------------------
source("titanic/codes/functions/makeGroup.R")
source("functions/generalized_op.R")
source("functions/mono_model.R")
source("functions/var2.R")
## ----global setting-----------------------------------------------------------
x1 = scale_x_continuous(breaks = seq(0, 80, 10), limits = c(0,80))
y1 = scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))

## ----load data-----------------------------------------------------------
getHdata(titanic3)

# Data Exploration
## ----descriptive statistics---------------------------------------------------
dat = titanic3 %>% select(survived, pclass, sex, age)

dat$dead = 1 - dat$survived
# label the new variable dead
label(dat$dead) = "dead"
describe (dat)

# remove missing
dat_nm = dat %>% filter(!is.na(age))
describe (dat_nm)

# table, display dead rate table
tab1 = dat_nm %>%
      group_by(pclass, dead) %>%
      summarise(n = n()) %>%
      mutate(prop = n / sum(n)) %>%
      arrange(desc(dead)) %>%
      select(pclass, prop)
tab1 = tab1[1:3, ]

print(xtable(t(tab1), digits = 3))

# divide age into several groups
quantile(dat_nm$age)

dat_nm = dat_nm %>%
      mutate(age_group = makeGroup(quantile(dat_nm$age), dat_nm$age))

## ----empirical risk and RR---------------------------------------------------
tmp.tab = dat_nm %>%
      group_by(pclass, age_group, sex, dead) %>%
      summarise(n = n()) %>%
      mutate(prop = n / sum(n)) %>%
      mutate(sd = sqrt(prop * (1-prop) / n)) %>%
      filter(dead == 1)

pr_emp.df = tmp.tab %>%
      ungroup() %>%
      select(sex, pclass,prop, sd)
# median age as x-axis
tmp = dat_nm %>%
      group_by(pclass, age_group, sex) %>%
      summarise(median_age = median(age))

med_age = tmp$median_age
pr_emp.df$age = med_age
pr_emp.df$methods = "emp"
# Risk
# tmp.tab %>% ggplot(aes(x=med_age, y=prop, color = sex, linetype = pclass)) +
#       geom_line(size = 1) +
#       geom_point(size = 2) +
#       geom_errorbar(aes(ymin = prop - sd, ymax =prop + sd), width = 2,position=position_dodge(0.02)) +
#      #x1  + y1 +
#       theme_bw()
# RR
tmp = tmp.tab %>%
      filter(pclass == "1st") %>%
      ungroup() %>%
      select(prop)

RR_emp.df = tmp.tab %>%
      filter(pclass == "1st") %>%
      ungroup() %>%
      select(age_group, sex)

for (i in c("2nd","3rd")) {

      tmp_i = tmp.tab %>%
            filter(pclass == i) %>%
            ungroup() %>%
            select(prop)

      RR_emp.df = data.frame(RR_emp.df, tmp_i / tmp)
}

# x-axis for RR. Take the median for each sex, and age group
tmp = dat_nm %>%
      group_by(age_group, sex) %>%
      summarise(med_age = median(age))

RR_emp.df$age = tmp$med_age

names(RR_emp.df)[c(3,4)] = c("Class2/Class1",  "Class3/Class1")
# RR_long.df = gather(RR.df, RR_level, RR, prop:prop.1, factor_key = TRUE)
# RR_long.df$RR_level = factor(RR_long.df$RR_level,
#                              levels = c("prop.1", "prop"),
#                              labels = c( "Class3/Class1", "Class2/Class1"))
#
# ggplot(RR_long.df, aes(x=med_age, y=RR, linetype = RR_level, color = sex)) +
#       geom_line(size = 1) +
#       geom_point(size = 2) +
#       ylab("Relative risk") + xlab("Age") +
#       #x1  + y2 +
#       theme_bw()


## ----regression---------------------------------------------------
dat_nm$pclass_num = recode(dat_nm$pclass, "1st"= 1, "2nd"=2, "3rd"=3)
dat_nm$sex_num = recode(dat_nm$sex, "male" = 1, "female"=0)
save(dat_nm, file = "titanic/data/data_remove_missing2.RData")

# build my prediction data
name = c("sex", "age", "pclass", "pr")
mydata_age = c(0 :80)
n_age = length(mydata_age)

mydata = data.frame(age = rep(mydata_age, 6),
                    sex = rep(rep(c("male", "female"), each = length(mydata_age)), 3),
                    pclass = rep(c("1st","2nd","3rd"), each = 2*length(mydata_age)))

mydata$pclass_num = recode(mydata$pclass, "1st"= 1, "2nd"=2, "3rd"=3)
mydata$sex_num = recode(mydata$sex, "male" = 1, "female"=0)

my_v1 = as.matrix(cbind(rep(1, nrow(mydata)),
                        select(mydata, sex_num, age),
                        select(mydata, sex_num) * select(mydata, age)))


save(mydata, my_v1, file = "titanic/data/mydata2.RData")

pa = 4
pb = 4
nz = 3

#---------------------------------------------------------#
# GLM Poisson
#---------------------------------------------------------#
coefs=c("pclass2nd","pclass2nd:sexmale","pclass2nd:age", "pclass2nd:sexmale:age",
        "pclass3rd","pclass3rd:sexmale","pclass3rd:age", "pclass3rd:sexmale:age")
poi.lm = glm(dead ~ pclass + sex + age + age * sex + pclass * sex + pclass * age  +  pclass * age * sex , family = poisson, data = dat_nm)
save(poi.lm, file = "titanic/data/poi_lm2.RData")

poi_coef = summary(poi.lm)$coef[coefs, c("Estimate")]
poi_sd = sqrt(diag(vcovHC(poi.lm, type="HC0")))[coefs]
#---------------------------------------------------------#
# Logistic
#---------------------------------------------------------#
logistic.lm = glm(dead ~ pclass + sex + age + age * sex + pclass * sex + pclass * age  +  pclass * age * sex , family = binomial, data = dat_nm)
save(logistic.lm, file = "titanic/data/logistic_lm2.RData")
#---------------------------------------------------------#
# MLE with Monotone
#---------------------------------------------------------#
v1 = as.matrix(cbind(rep(1, nrow(dat_nm)),
                     select(dat_nm, sex_num, age),
                     select(dat_nm, sex_num) * select(dat_nm, age)))
save(v1, file = "titanic/data/v1_2.RData")

mle_mono.md = max.likelihood.v2(y=dat_nm$dead, z = as.factor(dat_nm$pclass_num), va = v1, vb = v1, alpha.start = c(0,0,0,0), beta.start = c(0,0,0,0))

alpha_mle = mle_mono.md[1:pa]
beta_mle = mle_mono.md[(pa+1):(pa+pa)]
mle_mono_coef = c(alpha_mle, 2*alpha_mle)
# sd
mle_mono_sd = sqrt(rr.mono.var(y=dat_nm$dead, tr = as.factor(dat_nm$pclass_num), va = v1, vb = v1, gamma = alpha_mle, beta=beta_mle))[1:pa]

save(mle_mono.md, mle_mono_sd, file = "titanic/data/MLE_monotone2.RData")
#---------------------------------------------------------#
# GOP
#---------------------------------------------------------#
gop.md = max.likelihood.v3(y=dat_nm$dead, z = as.factor(dat_nm$pclass_num), va = v1, vb = v1, alpha.start = matrix(0, 4, 2), beta.start = rep(0,4))

gop.sd = round(sqrt(rr.gop.var(y=dat_nm$dead, tr = as.factor(dat_nm$pclass_num), va = v1, vb = v1, alpha=gop.md[[1]], beta=gop.md[[2]])),3)

save(gop.md, gop.sd, file = "titanic/data/GOP2.RData")

gop_coef  = as.vector(gop.md[[1]])

#------Estimation Table--------------
est.tab = round(data.frame(poi_coef, mle_mono_coef, gop_coef), 3)
colnames(est.tab) = c("Poisson", "MLE Mono", "GOP")
print(xtable(est.tab, digits = 3))
#------AIC--------------
AIC_tab = round(c(poi.lm$aic,
                  2 * mle_mono.md[pa+pb+2] + 2 * (pa+pb),
                  2 * gop.md[[4]] + 2 * (pa*2 + pb)),3)
names(AIC_tab) = c("Poisson", "MLE Mono", "GOP")

#------Risk Plots--------------
## prepare dataframe
mydata_glm = data.frame(mydata, pclass_num = rep(c(1:3), each = 2 * length(mydata_age)), pclass = rep(c("1st", "2nd", "3rd"), each = 2 * length(mydata_age)))

pred_glm.df = mydata_glm %>% dplyr::select(sex, age, pclass)

# poisson regressin
pred_glm.df$poi_pr = exp(predict(poi.lm, newdata = mydata_glm))

# logistic regression
pred_glm.df$logis_pr = exp(predict(logistic.lm, newdata = mydata_glm)) / (1 + exp(predict(logistic.lm, newdata = mydata_glm)))

# mle
Pzmin_Pzmax_est =  t(mapply(getProbScalarRR, my_v1 %*% alpha_mle * 2, my_v1 %*% beta_mle))

P.mat = matrix(0, ncol = nz, nrow = nrow(mydata))
P.mat[, c(1, nz)] = Pzmin_Pzmax_est
P.mat[, -c(1, nz)] = Pzmin_Pzmax_est[,1] * exp(my_v1 %*% alpha_mle  %*% t(c(1: (nz-2))))
colnames(P.mat) = c("1st", "2nd","3rd")

pred_mle.df = cbind(select(mydata, sex, age), P.mat)
pred_mle.df = pred_mle.df %>% distinct()
pred_mle_long.df = gather(pred_mle.df, key = "pclass", mle_mono_pr, "1st":"3rd")

# gop
alpha_gop = gop.md[[1]]
beta_gop = gop.md[[2]]
logRR.mat = my_v1 %*% alpha_gop
logOP.mat = my_v1 %*% beta_gop
P.mat = matrix(0, ncol = nz, nrow = nrow(mydata))
for(i in 1:nrow(mydata)){
      P.mat[i,] = getProbScalarRR_v2(logRR.mat[i,], logOP.mat[i])
}
colnames(P.mat) = c("1st", "2nd","3rd")
pred_gop.df = cbind(select(mydata, sex, age), P.mat) %>% distinct()
pred_gop_long.df = gather(pred_gop.df, key = "pclass", gop_pr, "1st":"3rd")

tmp = pred_gop_long.df %>% full_join(pred_mle_long.df, by = c("sex", "age", "pclass"))
pr.df = tmp %>% full_join(pred_glm.df, by = c("sex", "age", "pclass"))

pr_long.df = pr.df %>% gather(key = "methods", Pr, "gop_pr":"logis_pr")
pr_long.df = rbind(pr_long.df, select(pr_emp.df, sex, age, pclass, methods, age, Pr = prop))
pr_long.df$methods = factor(pr_long.df$methods,
                            levels = c("emp","poi_pr", "logis_pr", "mle_mono_pr", "gop_pr"),
                            labels = c("Empirical","Poisson", "Logistic", "Monotone", "Gop"))

pr_long.df %>%
      ggplot(aes(x = age, y = Pr, color = sex, linetype = pclass)) + geom_line(size = 1) +
      ylab("Death rate") +
      xlab("Age") +
      x1 + y1 +
      scale_color_discrete(guide=FALSE) + # remove legend
      scale_linetype_discrete(guid=FALSE) +
      theme_bw() +
      facet_wrap(. ~ methods, ncol = 2) +
      theme(strip.text.x = element_text(size=8, face="bold"),
            axis.text.x = element_text(color = "grey20", size = 8, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 8, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 8, face = "bold"),
            axis.title.y = element_text(color = "grey20", size = 8, face = "bold"))

ggsave(filename = "titanic/figures/pr-by-methods2.eps", width = 120, height = 200, units = "mm")
#----RR----------------------
RR.mat = matrix(NA, ncol = 7)
for (i in c("male", "female")){
      tmp_i = pr.df %>% select(sex, age) %>% filter(sex == i) %>% distinct()
      tmp_i = rbind(tmp_i, tmp_i)
      three_to_one = pr.df %>% filter(sex == i, pclass == "3rd") %>% select(gop_pr:logis_pr) / pr.df %>% filter(sex == i, pclass == "1st" ) %>% select(gop_pr:logis_pr)

      two_to_one = pr.df %>% filter(sex == i, pclass == "2nd") %>% select(gop_pr:logis_pr) / pr.df %>% filter(sex == i, pclass == "1st" ) %>% select(gop_pr:logis_pr)

      rr_tmp = cbind(rbind(two_to_one, three_to_one), RR_level = rep(c("Class2/Class1", "Class3/Class1"), each = length(mydata_age)))

      rr_i = cbind(tmp_i, rr_tmp) %>% as.matrix()
      RR.mat = rbind(RR.mat, rr_i)
}
RR.mat = RR.mat[-1,]

RR.df = as.data.frame(RR.mat)

RR_long.df = RR.df %>% gather(key = "methods", RR, "gop_pr":"logis_pr")
RR_long.df$age = as.numeric(RR_long.df$age)
emp_tmp = RR_emp.df %>%
      gather(key = "RR_level", RR, "Class2/Class1":"Class3/Class1") %>%
      select(sex, age, RR_level, RR) %>%
      mutate(methods = "emp")

RR_long.df = rbind(RR_long.df, emp_tmp)
RR_long.df$methods = factor(RR_long.df$methods,
                            levels = c("emp","poi_pr", "logis_pr", "mle_mono_pr", "gop_pr"),
                            labels = c("Empirical","Poisson", "Logistic", "Monotone", "Gop"))
RR_long.df$RR_level = factor(RR_long.df$RR_level,
                             levels = c( "Class3/Class1", "Class2/Class1"))
RR_long.df$RR = as.numeric(RR_long.df$RR)
RR_long.df$age = as.numeric(as.character(RR_long.df$age ))

RR_long.df %>%
      ggplot(aes(x = age, y = RR, color = sex, linetype = RR_level)) + geom_line(size = 1) +
      ylab("Relative death rate") +
      xlab("Age") +
      x1 +
      scale_color_discrete(guide=FALSE) + # remove legend
      scale_linetype_discrete(guid=FALSE) +
      theme_bw() +
      facet_wrap(. ~ methods, ncol = 2) +
      theme(strip.text.x = element_text(size=8, face="bold"),
            axis.text.x = element_text(color = "grey20", size = 8, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 8, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 8, face = "bold"),
            axis.title.y = element_text(color = "grey20", size = 8, face = "bold"))
ggsave(filename = "titanic/figures/rr-by-methods2.eps", width = 120, height = 200, units = "mm")
