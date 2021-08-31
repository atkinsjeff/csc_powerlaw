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
  summarise(transect.length = sum(transect.length))%>%
  data.frame() -> cst

names(cst) <- gsub(x = names(cst), pattern = "\\.", replacement = "_")  



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
plot(cst.test$transect_length, cst.test$rugosity)

x11()
plot(cst.test$transect_length, cst.test$moch)

x11()
plot(cst.test$rugosity/cst.test$transect_length, cst.test$moch/cst.test$transect_length)

cst.test %>%
  group_by(siteID) %>%
  summarise(transect.length = sum(transect_length, na.rm = TRUE))
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
summary(aov(log10(enl) ~ log10(can_max_ht) * pft, data = cst))
TukeyHSD(aov(log10(enl) ~ log10(can_max_ht) * pft, data = cst), "pft")


#### FHD
summary(aov(log10(fhd) ~ log10(can_max_ht) * pft, data = cst))
TukeyHSD(aov(log10(fhd) ~ log10(can_max_ht) * pft, data = cst), "pft")

summary(aov(log10(fhd) ~ log10(moch) * pft, data = cst))
TukeyHSD(aov(log10(fhd) ~ log10(moch) * pft, data = cst), "pft")


summary(aov(log10(rugosity) ~ log10(can_max_ht) * pft, data = cst))
TukeyHSD(aov(log10(rugosity) ~ log10(can_max_ht) * pft, data = cst), "pft")

summary(aov(log10(rugosity) ~ log10(moch) * pft, data = cst))
TukeyHSD(aov(log10(rugosity) ~ log10(moch) * pft, data = cst), "pft")


summary(aov(log10(enl) ~ log10(moch) * pft, data = cst))
TukeyHSD(aov(log10(enl) ~ log10(moch) * pft, data = cst), "pft")

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

# Rc ~ Hmax 
summary(lm(log10(rugosity) ~ log10(can_max_ht), data = DBF))
confint(lm(log10(rugosity) ~ log10(can_max_ht), data = DBF))



summary(lm(log10(rugosity) ~ log10(can_max_ht), data = ENF))
confint(lm(log10(rugosity) ~ log10(can_max_ht), data = ENF))


summary(lm(log10(rugosity) ~ log10(can_max_ht), data = MF))
confint(lm(log10(rugosity) ~ log10(can_max_ht), data = MF))

# FHD ~ Hmax
summary(lm(log10(fhd) ~ log10(can_max_ht), data = BDF))
confint(lm(log10(fhd) ~ log10(can_max_ht), data = BDF))

summary(lm(log10(fhd) ~ log10(can_max_ht), data = ENF))
confint(lm(log10(fhd) ~ log10(can_max_ht), data = ENF))

summary(lm(log10(fhd) ~ log10(can_max_ht), data = MF))
confint(lm(log10(fhd) ~ log10(can_max_ht), data = MF))

# ENL ~ Hmax
summary(lm(log10(enl) ~ log10(can_max_ht), data = BDF))
confint(lm(log10(enl) ~ log10(can_max_ht), data = BDF))

summary(lm(log10(enl) ~ log10(can_max_ht), data = ENF))
confint(lm(log10(enl) ~ log10(can_max_ht), data = ENF))

summary(lm(log10(enl) ~ log10(can_max_ht), data = MF))
confint(lm(log10(enl) ~ log10(can_max_ht), data = MF))
#write.csv(cst, "power_law_csc_subset_20201110.csv")

# Rc ~ Hmax 
summary(lm(log10(rugosity) ~ log10(moch), data = DBF))
confint(lm(log10(rugosity) ~ log10(moch), data = DBF))


summary(lm(log10(rugosity) ~ log10(moch), data = ENF))
confint(lm(log10(rugosity) ~ log10(moch), data = ENF))


summary(lm(log10(rugosity) ~ log10(moch), data = MF))
confint(lm(log10(rugosity) ~ log10(moch), data = MF))

# FHD ~ Hmax
summary(lm(log10(fhd) ~ log10(moch), data = BDF))
confint(lm(log10(fhd) ~ log10(moch), data = BDF))

summary(lm(log10(fhd) ~ log10(moch), data = ENF))
confint(lm(log10(fhd) ~ log10(moch), data = ENF))

summary(lm(log10(fhd) ~ log10(moch), data = MF))
confint(lm(log10(fhd) ~ log10(moch), data = MF))

# ENL ~ Hmax
summary(lm(log10(enl) ~ log10(moch), data = BDF))
confint(lm(log10(enl) ~ log10(moch), data = BDF))

summary(lm(log10(enl) ~ log10(moch), data = ENF))
confint(lm(log10(enl) ~ log10(moch), data = ENF))

summary(lm(log10(enl) ~ log10(moch), data = MF))
confint(lm(log10(enl) ~ log10(moch), data = MF))




