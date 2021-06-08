## Do analyses on the power-law relationship between catchment area and lake surface area
# 
# rm(list=ls())

library(lmerTest)
library(car)
library(MuMIn)
library(RColorBrewer)

# data
csc <- read.csv("./data/pcl_output_20201109.csv") 
#pft <- read.csv("neon_pft_plots.csv")
csc$pft[csc$pft == "BDF"] <- "DBF"

# 
unique(csc$siteID)

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


# what I’d do is fit 
# 1) model without any random effects 
rug.max.ht <- lm(log10(rugosity) ~ log10(can_max_ht), data = cst)

# 2) model with random intercept 
rug.max.ht.int <- lmer(log10(rugosity) ~ log10(can_max_ht) + (1|pft), data = cst, REML = FALSE)

# 3) model with random slope 
rug.max.ht.slope <- lmer(log10(rugosity) ~ (log10(can_max_ht)|pft), data = cst, REML = FALSE)

# 4) model with random slope and random intercept, see which has lowest AIC
rug.max.ht.int.slope <- lmer(log10(rugosity) ~ (log10(can_max_ht)|pft) + (1|pft), data = cst, REML = FALSE)

# AICc values
AICc(rug.max.ht)
AICc(rug.max.ht.int)
AICc(rug.max.ht.slope)
AICc(rug.max.ht.int.slope)

ranef(rug.max.ht.int.slope)

anova(rug.max.ht, rug.max.ht.int, rug.max.ht.slope, rug.max.ht.int.slope)


#####
library(lsmeans)
model <- lm(log10(rugosity) ~ log10(can_max_ht)*pft, data = cst)
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

