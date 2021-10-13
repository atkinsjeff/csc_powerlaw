## Do analyses on the power-law relationship of canopy height vs complexity
# rm(list=ls())

library(lmerTest)
library(car)
library(MuMIn)
library(RColorBrewer)
require(tidyverse)
library(lsmeans)

# DATA IMPRT AND CLEANING
csc <- read.csv("./data/pcl_output_20201109.csv") 
#pft <- read.csv("neon_pft_plots.csv")
csc$pft[csc$pft == "BDF"] <- "DBF"

# cleaning
csc %>%
  filter(cover.fraction > 25) %>%
  filter(can.max.ht < 50) %>%
  filter(management == "unmanaged") %>%
  filter(disturbance == "") %>%
  filter(plotID != "?" & plotID != "") %>%
  select(siteID, plotID, transect.length, pft, moch, can.max.ht, rugosity, 
         enl, fhd) %>%
  group_by(plotID, siteID, pft) %>%
  summarise_at(.vars = vars(moch, can.max.ht, rugosity, enl, fhd),
               .funs = "mean") %>%
  data.frame() -> cst

names(cst) <- gsub(x = names(cst), pattern = "\\.", replacement = "_")  


#### LENGTH COMPARISON
csc %>%
  filter(cover.fraction > 25) %>%
  filter(can.max.ht < 50) %>%
  filter(management == "unmanaged") %>%
  filter(disturbance == "") %>%
  filter(plotID != "?" & plotID != "") %>%
  select(siteID, plotID, transect.length, pft) %>%
  group_by(plotID, siteID, pft) %>%
  summarise(transect.length = sum(transect.length, na.rm = TRUE))%>%
  data.frame() -> cst.length
 
names(cst.length) <- gsub(x = names(cst.length), pattern = "\\.", replacement = "_")  

# test for differences in transect length

# number of transects analysis.
csc %>%
  filter(cover.fraction > 25) %>%
  filter(can.max.ht < 50) %>%
  filter(management == "unmanaged") %>%
  filter(disturbance == "") -> no.count
dim(no.count)

cst.test <- merge(cst.length, cst)

x11()
plot(cst.test$transect.length, cst.test$rugosity)

x11()
plot(cst.test$transect.length, cst.test$moch)

x11()
plot(cst.test$rugosity/cst.test$transect.length, cst.test$moch/cst.test$transect.length)

cst.test %>%
  group_by(siteID) %>%
  summarise(transect.length = sum(transect.length, na.rm = TRUE))
# 
# # 1) model without any random effects 
# rug.max.ht <- lm(log10(rugosity) ~ log10(can_max_ht), data = cst)
# 
# # 2) model with random intercept 
# rug.max.ht.int <- lmer(log10(rugosity) ~ log10(can_max_ht) + (1|pft), data = cst, REML = FALSE)
# 
# # 3) model with random slope 
# rug.max.ht.slope <- lmer(log10(rugosity) ~ (log10(can_max_ht)|pft), data = cst, REML = FALSE)
# 
# # 4) model with random slope and random intercept, see which has lowest AIC
# rug.max.ht.int.slope <- lmer(log10(rugosity) ~ (log10(can_max_ht)|pft) + (1|pft), data = cst, REML = FALSE)
# 
# # AICc values
# AICc(rug.max.ht)
# AICc(rug.max.ht.int)
# AICc(rug.max.ht.slope)
# AICc(rug.max.ht.int.slope)
# 
# ranef(rug.max.ht.int.slope)
# TukeyHSD(aov.fhd, "pft")

# ancova results

# ENL
summary(aov(log10(enl) ~ log10(can_max_ht) * pft, data = cst))
TukeyHSD(aov(log10(enl) ~ log10(can_max_ht) * pft, data = cst), "pft")

summary(aov(log10(enl) ~ log10(moch) * pft, data = cst))
TukeyHSD(aov(log10(enl) ~ log10(moch) * pft, data = cst), "pft")

#### FHD
summary(aov(log10(fhd) ~ log10(can_max_ht) * pft, data = cst))
TukeyHSD(aov(log10(fhd) ~ log10(can_max_ht) * pft, data = cst), "pft")

summary(aov(log10(fhd) ~ log10(moch) * pft, data = cst))
TukeyHSD(aov(log10(fhd) ~ log10(moch) * pft, data = cst), "pft")

#### RUGOSITY
summary(aov(log10(rugosity) ~ log10(can_max_ht) * pft, data = cst))
TukeyHSD(aov(log10(rugosity) ~ log10(can_max_ht) * pft, data = cst), "pft")

summary(aov(log10(rugosity) ~ log10(moch) * pft, data = cst))
TukeyHSD(aov(log10(rugosity) ~ log10(moch) * pft, data = cst), "pft")




summary(aov(log10(fhd) ~ log10(moch) * pft, data = cst))
TukeyHSD(aov(log10(fhd) ~ log10(moch) * pft, data = cst), "pft")

summary(aov(log10(rugosity) ~ log10(moch) * pft, data = cst))
TukeyHSD(aov(log10(rugosity) ~ log10(moch) * pft, data = cst), "pft")


#####
model <- lm(log10(rugosity) ~ log10(can_max_ht)*pft, data = cst)
print(model)
m.lst <- lstrends(model, "pft", var = "can_max_ht")
pairs(m.lst)

model <- lm(log10(enl) ~ log10(can_max_ht)*pft, data = cst)
m.lst <- lstrends(model, "pft", var = "can_max_ht")
pairs(m.lst)

model <- lm(enl ~ can_max_ht*pft, data = cst)
m.lst <- lstrends(model, "pft", var = "can_max_ht")
pairs(m.lst)

model <- lm(log10(fhd) ~ log10(can_max_ht)*pft, data = cst)
m.lst <- lstrends(model, "pft", var = "can_max_ht")
pairs(m.lst)


# INTERCEPTS
model <- lm(log10(rugosity) ~ log10(can_max_ht)*pft, data = cst)
m.lst <- lsmeans(model, "pft", var = "can_max_ht")
pairs(m.lst)

model <- lm(log10(enl) ~ log10(can_max_ht)*pft, data = cst)
m.lst <- lsmeans(model, "pft", var = "can_max_ht")
pairs(m.lst)

model <- lm(log10(fhd) ~ log10(can_max_ht)*pft, data = cst)
m.lst <- lsmeans(model, "pft", var = "can_max_ht")
pairs(m.lst)





# PFT specific datasets
cst %>%
  filter(pft == "DBF") %>%
  data.frame() -> DBF

cst %>%
  filter(pft == "MF") %>%
  data.frame() -> MF

cst %>%
  filter(pft == "ENF") %>%
  data.frame() -> ENF

### OLS regressions

#####    H_max

# ALL
summary(lm(log10(rugosity) ~ log10(can_max_ht), data = cst))
confint(lm(log10(rugosity) ~ log10(can_max_ht), data = cst))

lm.rugosity.can_max_ht<-lm(log10(rugosity) ~ log10(can_max_ht), data = cst)  
qm.rugosity.can_max_ht<-lm(log10(rugosity) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = cst)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ cst$can_max_ht)

# if p < 0.05 FAILS linearity test
anova(lm.rugosity.can_max_ht, qm.rugosity.can_max_ht) # passes linearity test         
# if P <0,05, FAILS heteroskedasticity test
summary(rm.rugosity.can_max_ht)  # FAILS heteroskedasticity


########## Rc ~ Hmax 
# DBF
summary(lm(log10(rugosity) ~ log10(can_max_ht), data = DBF))
confint(lm(log10(rugosity) ~ log10(can_max_ht), data = DBF))

lm.rugosity.can_max_ht<-lm(log10(rugosity) ~ log10(can_max_ht), data = DBF)  
qm.rugosity.can_max_ht<-lm(log10(rugosity) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = DBF)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ DBF$can_max_ht)

# if p < 0.05 FAILS linearity test
anova(lm.rugosity.can_max_ht, qm.rugosity.can_max_ht) #passes linearity test         
# if P <0,05, FAILS heteroskedasticity test
summary(rm.rugosity.can_max_ht) #passes heteroskedasticity test


# ENF
summary(lm(log10(rugosity) ~ log10(can_max_ht), data = ENF))
confint(lm(log10(rugosity) ~ log10(can_max_ht), data = ENF))

lm.rugosity.can_max_ht<-lm(log10(rugosity) ~ log10(can_max_ht), data = ENF)  
qm.rugosity.can_max_ht<-lm(log10(rugosity) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = ENF)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ ENF$can_max_ht)

anova(lm.rugosity.can_max_ht, qm.rugosity.can_max_ht) #passes linearity test         
summary(rm.rugosity.can_max_ht) #passes heteroskedasticity test

# MF 
summary(lm(log10(rugosity) ~ log10(can_max_ht), data = MF))
confint(lm(log10(rugosity) ~ log10(can_max_ht), data = MF))

lm.rugosity.can_max_ht<-lm(log10(rugosity) ~ log10(can_max_ht), data = MF)  
qm.rugosity.can_max_ht<-lm(log10(rugosity) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = MF)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ MF$can_max_ht)

anova(lm.rugosity.can_max_ht, qm.rugosity.can_max_ht) #passes linearity test         
summary(rm.rugosity.can_max_ht) #passes heteroskedasticity test





######## FHD ~ Hmax

# ALL
summary(lm(log10(fhd) ~ log10(can_max_ht), data = cst))
confint(lm(log10(fhd) ~ log10(can_max_ht), data = cst))

lm.fhd.can_max_ht<-lm(log10(fhd) ~ log10(can_max_ht), data = cst)  
qm.fhd.can_max_ht<-lm(log10(fhd) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = cst)
rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ cst$can_max_ht)

anova(lm.fhd.can_max_ht, qm.fhd.can_max_ht) # fails linearity test         
summary(rm.fhd.can_max_ht)  # passes heteroskedasticity

# DBF
summary(lm(log10(fhd) ~ log10(can_max_ht), data = DBF))
confint(lm(log10(fhd) ~ log10(can_max_ht), data = DBF))

lm.fhd.can_max_ht<-lm(log10(fhd) ~ log10(can_max_ht), data = DBF)  
qm.fhd.can_max_ht<-lm(log10(fhd) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = DBF)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ DBF$can_max_ht)

anova(lm.fhd.can_max_ht, qm.fhd.can_max_ht) #fails linearity test         
summary(rm.fhd.can_max_ht) #passes heteroskedasticity test

# ENF
summary(lm(log10(fhd) ~ log10(can_max_ht), data = ENF))
confint(lm(log10(fhd) ~ log10(can_max_ht), data = ENF))

lm.fhd.can_max_ht<-lm(log10(fhd) ~ log10(can_max_ht), data = ENF)  
qm.fhd.can_max_ht<-lm(log10(fhd) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = ENF)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ ENF$can_max_ht)

anova(lm.fhd.can_max_ht, qm.fhd.can_max_ht) #passes linearity test         
summary(rm.fhd.can_max_ht) # passes heteroskedasticity test

# MF
summary(lm(log10(fhd) ~ log10(can_max_ht), data = MF))
confint(lm(log10(fhd) ~ log10(can_max_ht), data = MF))

lm.fhd.can_max_ht<-lm(log10(fhd) ~ log10(can_max_ht), data = MF)  
qm.fhd.can_max_ht<-lm(log10(fhd) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = MF)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ MF$can_max_ht)

anova(lm.fhd.can_max_ht, qm.fhd.can_max_ht) #passes linearity test         
summary(rm.fhd.can_max_ht) # passes heteroskedasticity test




####### ENL ~ Hmax

# ALL
summary(lm(log10(enl) ~ log10(can_max_ht), data = cst))
confint(lm(log10(enl) ~ log10(can_max_ht), data = cst))

lm.enl.can_max_ht<-lm(log10(enl) ~ log10(can_max_ht), data = cst)  
qm.enl.can_max_ht<-lm(log10(enl) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = cst)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ cst$can_max_ht)

anova(lm.enl.can_max_ht, qm.enl.can_max_ht) # passeslinearity test         
summary(rm.enl.can_max_ht) # fails heteroskedasticity test


# DBF
summary(lm(log10(enl) ~ log10(can_max_ht), data = DBF))
confint(lm(log10(enl) ~ log10(can_max_ht), data = DBF))

lm.enl.can_max_ht<-lm(log10(enl) ~ log10(can_max_ht), data = DBF)  
qm.enl.can_max_ht<-lm(log10(enl) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = DBF)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ DBF$can_max_ht)

anova(lm.enl.can_max_ht, qm.enl.can_max_ht) # passes linearity test         
summary(rm.enl.can_max_ht) # passes heteroskedasticity test

# ENF
summary(lm(log10(enl) ~ log10(can_max_ht), data = ENF))
confint(lm(log10(enl) ~ log10(can_max_ht), data = ENF))

lm.enl.can_max_ht<-lm(log10(enl) ~ log10(can_max_ht), data = ENF)  
qm.enl.can_max_ht<-lm(log10(enl) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = ENF)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ ENF$can_max_ht)

anova(lm.enl.can_max_ht, qm.enl.can_max_ht) # passes linearity test         
summary(rm.enl.can_max_ht) #fails heteroskedasticity test

# MF
summary(lm(log10(enl) ~ log10(can_max_ht), data = MF))
confint(lm(log10(enl) ~ log10(can_max_ht), data = MF))

lm.enl.can_max_ht<-lm(log10(enl) ~ log10(can_max_ht), data = MF)  
qm.enl.can_max_ht<-lm(log10(enl) ~ log10(can_max_ht) + I(log10(can_max_ht)^2), data = MF)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ MF$can_max_ht)

anova(lm.enl.can_max_ht, qm.enl.can_max_ht) # passes linearity test         
summary(rm.enl.can_max_ht) # passes heteroskedasticity test





################################################

### MOCH
# Rc ~ MOCH

# ALL
summary(lm(log10(rugosity) ~ log10(moch), data = cst))
confint(lm(log10(rugosity) ~ log10(moch), data = cst))

lm.rugosity.moch<-lm(log10(rugosity) ~ log10(moch), data = cst)  
qm.rugosity.moch<-lm(log10(rugosity) ~ log10(moch) + I(log10(moch)^2), data = cst)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ cst$moch)

# if p < 0.05 FAILS linearity test
anova(lm.rugosity.moch, qm.rugosity.moch) # passes linearity test         
# if P <0,05, FAILS heteroskedasticity test
summary(rm.rugosity.moch) #fails heteroskedasticity test



# DBF
summary(lm(log10(rugosity) ~ log10(moch), data = DBF))
confint(lm(log10(rugosity) ~ log10(moch), data = DBF))

lm.rugosity.moch<-lm(log10(rugosity) ~ log10(moch), data = DBF)  
qm.rugosity.moch<-lm(log10(rugosity) ~ log10(moch) + I(log10(moch)^2), data = DBF)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ DBF$moch)

# if p < 0.05 FAILS linearity test
anova(lm.rugosity.moch, qm.rugosity.moch) #passes linearity test         
# if P <0,05, FAILS heteroskedasticity test
summary(rm.rugosity.moch) #fails heteroskedasticity test


# ENF
summary(lm(log10(rugosity) ~ log10(moch), data = ENF))
confint(lm(log10(rugosity) ~ log10(moch), data = ENF))

lm.rugosity.moch<-lm(log10(rugosity) ~ log10(moch), data = ENF)  
qm.rugosity.moch<-lm(log10(rugosity) ~ log10(moch) + I(log10(moch)^2), data = ENF)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ ENF$moch)

anova(lm.rugosity.moch, qm.rugosity.moch) #passes linearity test         
summary(rm.rugosity.moch) #passes heteroskedasticity test

# MF 
summary(lm(log10(rugosity) ~ log10(moch), data = MF))
confint(lm(log10(rugosity) ~ log10(moch), data = MF))

lm.rugosity.moch<-lm(log10(rugosity) ~ log10(moch), data = MF)  
qm.rugosity.moch<-lm(log10(rugosity) ~ log10(moch) + I(log10(moch)^2), data = MF)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ MF$moch)

anova(lm.rugosity.moch, qm.rugosity.moch) #passes linearity test         
summary(rm.rugosity.moch) #passes heteroskedasticity test





######## FHD ~ MOCH

# ALL
summary(lm(log10(fhd) ~ log10(moch), data = cst))
confint(lm(log10(fhd) ~ log10(moch), data = cst))

lm.fhd.moch<-lm(log10(fhd) ~ log10(moch), data = cst)  
qm.fhd.moch<-lm(log10(fhd) ~ log10(moch) + I(log10(moch)^2), data = cst)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ cst$moch)

anova(lm.fhd.moch, qm.fhd.moch) # passes linearity test         
summary(rm.fhd.moch) # passes heteroskedasticity

# DBF
summary(lm(log10(fhd) ~ log10(moch), data = DBF))
confint(lm(log10(fhd) ~ log10(moch), data = DBF))

lm.fhd.moch<-lm(log10(fhd) ~ log10(moch), data = DBF)  
qm.fhd.moch<-lm(log10(fhd) ~ log10(moch) + I(log10(moch)^2), data = DBF)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ DBF$moch)

anova(lm.fhd.moch, qm.fhd.moch) # FAILS linearity test         
summary(rm.fhd.moch) #passes heteroskedasticity test

# ENF
summary(lm(log10(fhd) ~ log10(moch), data = ENF))
confint(lm(log10(fhd) ~ log10(moch), data = ENF))

lm.fhd.moch<-lm(log10(fhd) ~ log10(moch), data = ENF)  
qm.fhd.moch<-lm(log10(fhd) ~ log10(moch) + I(log10(moch)^2), data = ENF)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ ENF$moch)

anova(lm.fhd.moch, qm.fhd.moch) #passes linearity test         
summary(rm.fhd.moch) # FAILS heteroskedasticity test

# MF
summary(lm(log10(fhd) ~ log10(moch), data = MF))
confint(lm(log10(fhd) ~ log10(moch), data = MF))

lm.fhd.moch<-lm(log10(fhd) ~ log10(moch), data = MF)  
qm.fhd.moch<-lm(log10(fhd) ~ log10(moch) + I(log10(moch)^2), data = MF)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ MF$moch)

anova(lm.fhd.moch, qm.fhd.moch) # passes linearity test         
summary(rm.fhd.moch) # passes heteroskedasticity test




####### ENL ~ MOCH

# ALL
summary(lm(log10(enl) ~ log10(moch), data = cst))
confint(lm(log10(enl) ~ log10(moch), data = cst))

lm.enl.moch<-lm(log10(enl) ~ log10(moch), data = cst)  
qm.enl.moch<-lm(log10(enl) ~ log10(moch) + I(log10(moch)^2), data = cst)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ DBF$moch)

anova(lm.enl.moch, qm.enl.moch) # fails linearity test         
summary(rm.enl.moch) # passes heteroske

# DBF
summary(lm(log10(enl) ~ log10(moch), data = DBF))
confint(lm(log10(enl) ~ log10(moch), data = DBF))

lm.enl.moch<-lm(log10(enl) ~ log10(moch), data = DBF)  
qm.enl.moch<-lm(log10(enl) ~ log10(moch) + I(log10(moch)^2), data = DBF)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ DBF$moch)

anova(lm.enl.moch, qm.enl.moch) # passes linearity test         
summary(rm.enl.moch) # passes  heteroskedasticity test

# ENF
summary(lm(log10(enl) ~ log10(moch), data = ENF))
confint(lm(log10(enl) ~ log10(moch), data = ENF))

lm.enl.moch<-lm(log10(enl) ~ log10(moch), data = ENF)  
qm.enl.moch<-lm(log10(enl) ~ log10(moch) + I(log10(moch)^2), data = ENF)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ ENF$moch)

anova(lm.enl.moch, qm.enl.moch) # passes linearity test         
summary(rm.enl.moch) #fails heteroskedasticity test

# MF
summary(lm(log10(enl) ~ log10(moch), data = MF))
confint(lm(log10(enl) ~ log10(moch), data = MF))

lm.enl.moch<-lm(log10(enl) ~ log10(moch), data = MF)  
qm.enl.moch<-lm(log10(enl) ~ log10(moch) + I(log10(moch)^2), data = MF)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ MF$moch)

anova(lm.enl.moch, qm.enl.moch) # fails linearity test         
summary(rm.enl.moch) #passesheteroskedasticity test