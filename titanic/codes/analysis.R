rm(list = ls())

library(ggplot2)
library(dplyr)
library(brm)
library(tidyr)
library(xtable)
library(rms)
## ----load functions-----------------------------------------------------------
source("titanic/codes/functions/makeGroup.R")
source("functions/generalized_op.R")
source("functions/mono_model.R")
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
      select(pclass, sex)
# median age as x-axis
tmp = dat_nm %>%
      group_by(pclass, age_group, sex) %>%
      summarise(median_age = median(age))

med_age = tmp$median_age
pr_emp.df$med_age = med_age

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

RR_emp.df$med_age = tmp$med_age

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
dat_nm$age_sq = dat_nm$age^2

# build my prediction data
name = c("sex", "age", "pclass", "pr")
mydata_age = c(0 :80)
n_age = length(mydata_age)

mydata = data.frame(age = rep(mydata_age, 6),
                    sex = rep(rep(c("male", "female"), each = length(mydata_age)), 3),
                    pclass = rep(c("1st","2nd","3rd"), each = 2*length(mydata_age)))

mydata$age_sq = mydata$age^2
mydata$pclass_num = recode(mydata$pclass, "1st"= 1, "2nd"=2, "3rd"=3)
mydata$sex_num = recode(mydata$sex, "male" = 1, "female"=0)

my_v1 = as.matrix(cbind(rep(1, nrow(mydata)),
                        select(mydata, sex_num, age, age_sq)))

save(mydata, my_v1, file = "titanic/data/mydata.RData")


#---------------------------------------------------------#
# GLM Poisson
#---------------------------------------------------------#
poi.lm = glm(dead ~ pclass + sex + age + age_sq + age * sex + pclass * sex + pclass * age + pclass * age_sq +  pclass * age * sex , family = poisson, data = dat_nm)
save(poi.lm, file = "titanic/data/poi_lm.RData")

poi_coef = summary(poi.lm)$coef[c(coefs), c("Estimate", "Std. Error")]
#---------------------------------------------------------#
# Logistic
#---------------------------------------------------------#
logistic.lm = glm(dead ~ pclass + sex + age + age_sq + age * sex + pclass * sex + pclass * age + pclass * age_sq +  pclass * age * sex , family = poisson, data = dat_nm)
save(logistic.lm, file = "titanic/data/logistic_lm.RData")
#---------------------------------------------------------#
# MLE with Monotone
#---------------------------------------------------------#
v1 = as.matrix(cbind(rep(1, nrow(dat_nm)),
                     select(dat_nm, sex_num, age, age_sq),
                     select(dat_nm, sex_num) * select(dat_nm, age)))
mle_mono.md = max.likelihood.v2(y=dat_nm$dead, z = as.factor(dat_nm$pclass_num), va = v1, vb = v1, alpha.start = c(0,0,0,0,0), beta.start = c(0,0,0,0,0))
save(mle_mono.md, file = "titanic/data/MLE_monotone.RData")
#---------------------------------------------------------#
# GOP
#---------------------------------------------------------#
gop.md = max.likelihood.v3(y=dat_nm$dead, z = as.factor(dat_nm$pclass_num), va = v1, vb = v1, alpha.start = matrix(0, 5, 2), beta.start = rep(0,5))
save(gop.md, file = "titanic/data/titaninc_GOP.RData")
