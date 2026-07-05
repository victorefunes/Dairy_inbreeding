library(tidyverse)
library(statar)
library(fixest)
library(broom)
library(haven)
library(CMatching)

data("schools")

schools |>
  mutate(T = ifelse(homework > 1, 1, 0)) ->
  schools

addmargins(table(schools_ps$schid, schools_ps$T))

pmodel <- glm(T~ses+as.factor(sex)+white+public, data = schools, 
            family=binomial(link="logit")) 

pmodel |>
  augment(data = schools, type.predict = "response") |>
  rename(eps = .fitted) ->
  schools_ps

psm_w <- CMatch(type="within", Y=schools_ps$math, Tr=schools_ps$T, 
                X=schools_ps$eps, Group=schools_ps$schid)

# percentage of drops 
psm_w$ndrops/psm_w$orig.treated.nobs

# percentage of drops by school 
psm_w$orig.dropped.nobs.by.group/table(schools_ps$schid)

b_psm_w <- CMatchBalance(T~ses + as.factor(sex) + white + public, 
                         data=schools, match.out=psm_w)

vec <- vector() 
for(i in 1:length(b_psm_w$AfterMatching)){
  vec[[i]] <- b_psm_w$AfterMatching[[i]]$sdiff
} 
mean(abs(vec))

# pooled matching 
pm <- Match(Y=NULL, Tr=schools_ps$T, X=schools_ps$eps, caliper=2)

#same output as before (with a warning about the absence of groups, 
#ties=FALSE,replace=FALSE) 
pm <- CMatch(type="within", Y=NULL, Tr=schools_ps$T, X=schools_ps$eps,
             Group=NULL, caliper=2)

# within matching 
wm <- CMatch(type="within", Y=NULL, Tr=schools_ps$T, X=schools_ps$eps,
            Group=schools_ps$schid, caliper=2)
summary(wm)

# preferential within matching 
pwm <- CMatch(type="pwithin", Y=NULL, Tr=schools_ps$T, X=schools_ps$eps,
             Group=schools_ps$schid, caliper=2)

CMatchBalance(schools_ps$T~schools_ps$eps, match.out=wm)

psm_w$After[[1]]["sdiff"]

b_psm_w$After[[1]]["sdiff"]


psm_pw <- CMatch(type="pwithin", Y=schools_ps$math, Tr=schools_ps$T, 
                 X=schools_ps$eps, Group=schools_ps$schid)
summary(psm_pw)