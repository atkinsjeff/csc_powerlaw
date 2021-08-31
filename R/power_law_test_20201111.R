## test power law scaling between canopy height and canopy complexity

rm(list=ls())

library(dplyr)

dat<-read.csv("~/Box Sync/ForestComplexity/PowerLawScaling/power_law_csc_subset_20201110.csv",
              stringsAsFactors = F)

dat<-dat[dat$plotID!="" & dat$plotID!="?",]

## canopy height variables are 'moch' and 'can_max_ht'
## complexity variables are rugosity, top rugosity, enl, fhd, gini, and rumple

## in update, we only consider rugosity, enl, and fhd; we also consider 
## whether things are consistent when we use transects or plots as the spatial unit

dat<-dat[,!colnames(dat) %in% c("top_rugosity","gini","rumple")]

logdat<-dat
logdat$moch<-log10(dat$moch)
logdat$can_max_ht<-log10(dat$can_max_ht)
logdat$rugosity<-log10(dat$rugosity)
logdat$enl<-log10(dat$enl)
logdat$fhd<-log10(dat$fhd)



## here, moch is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.moch<-lm(rugosity ~ moch, data=logdat)
qm.rugosity.moch<-lm(rugosity ~ moch + I(moch^2), data=logdat)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ logdat$moch)

anova(lm.rugosity.moch,qm.rugosity.moch) #passes linearity test                    
summary(rm.rugosity.moch) #fails heteroskedasticity test


#enl
lm.enl.moch<-lm(enl ~ moch, data=logdat)
qm.enl.moch<-lm(enl ~ moch + I(moch^2), data=logdat)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ logdat$moch)

anova(lm.enl.moch,qm.enl.moch) #fails linearity test                    
summary(rm.enl.moch) #fails heteroskedasticity test

#fhd
lm.fhd.moch<-lm(fhd ~ moch, data=logdat)
qm.fhd.moch<-lm(fhd ~ moch + I(moch^2), data=logdat)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ logdat$moch)

anova(lm.fhd.moch,qm.fhd.moch) #passes linearity test                    
summary(rm.fhd.moch) #fails heteroskedasticity test

summary(lm.fhd.moch) #exponent = 0.27; significantly different than 1
plot(logdat$moch, logdat$fhd, pch=16)

## here, can_max_ht is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht, data=logdat)
qm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht + I(can_max_ht^2), data=logdat)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ logdat$can_max_ht)

anova(lm.rugosity.can_max_ht,qm.rugosity.can_max_ht) #fails linearity test                    
summary(rm.rugosity.can_max_ht) #fails heteroskedasticity test

#enl
lm.enl.can_max_ht<-lm(enl ~ can_max_ht, data=logdat)
qm.enl.can_max_ht<-lm(enl ~ can_max_ht + I(can_max_ht^2), data=logdat)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ logdat$can_max_ht)

anova(lm.enl.can_max_ht,qm.enl.can_max_ht) #passes linearity test                    
summary(rm.enl.can_max_ht) #fails heteroskedasticity test

#fhd
lm.fhd.can_max_ht<-lm(fhd ~ can_max_ht, data=logdat)
qm.fhd.can_max_ht<-lm(fhd ~ can_max_ht + I(can_max_ht^2), data=logdat)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ logdat$can_max_ht)

anova(lm.fhd.can_max_ht,qm.fhd.can_max_ht) #fails linearity test                    
summary(rm.fhd.can_max_ht) #passes heteroskedasticity test


## --------------------------------------------------------------------------
## Separate out plant functional types
# ---------------------------------------------------------------------------

## BDF ---------------------------------------------------------------------------------------

## here, moch is the canopy height variable --------------------------------------------------

logdat.bdf<-logdat[logdat$pft=="DBF",]

#rugosity
lm.rugosity.moch<-lm(rugosity ~ moch, data=logdat.bdf)
qm.rugosity.moch<-lm(rugosity ~ moch + I(moch^2), data=logdat.bdf)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ logdat.bdf$moch)

anova(lm.rugosity.moch,qm.rugosity.moch) #passes linearity test                    
summary(rm.rugosity.moch) #fails heteroskedasticity test

summary(lm.rugosity.moch) #exponent = 1.44

#enl
lm.enl.moch<-lm(enl ~ moch, data=logdat.bdf)
qm.enl.moch<-lm(enl ~ moch + I(moch^2), data=logdat.bdf)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ logdat.bdf$moch)

anova(lm.enl.moch,qm.enl.moch) #passes linearity test                    
summary(rm.enl.moch) #passes heteroskedasticity test

summary(lm.enl.moch)

#fhd
lm.fhd.moch<-lm(fhd ~ moch, data=logdat.bdf)
qm.fhd.moch<-lm(fhd ~ moch + I(moch^2), data=logdat.bdf)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ logdat.bdf$moch)

anova(lm.fhd.moch,qm.fhd.moch) #fails linearity test                    
summary(rm.fhd.moch) #passes heteroskedasticity test


## here, can_max_ht is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht, data=logdat.bdf)
qm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht + I(can_max_ht^2), data=logdat.bdf)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ logdat.bdf$can_max_ht)

anova(lm.rugosity.can_max_ht,qm.rugosity.can_max_ht) #passes linearity test         
summary(rm.rugosity.can_max_ht) #passes heteroskedasticity test

summary(lm.rugosity.can_max_ht) #exponent = 2.54

#enl
lm.enl.can_max_ht<-lm(enl ~ can_max_ht, data=logdat.bdf)
qm.enl.can_max_ht<-lm(enl ~ can_max_ht + I(can_max_ht^2), data=logdat.bdf)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ logdat.bdf$can_max_ht)

anova(lm.enl.can_max_ht,qm.enl.can_max_ht) #passes linearity test                    
summary(rm.enl.can_max_ht) # fails heteroskedasticity test

summary(lm.enl.can_max_ht)# exponent = 0.93, not different from 1

#fhd
lm.fhd.can_max_ht<-lm(fhd ~ can_max_ht, data=logdat.bdf)
qm.fhd.can_max_ht<-lm(fhd ~ can_max_ht + I(can_max_ht^2), data=logdat.bdf)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ logdat.bdf$can_max_ht)

anova(lm.fhd.can_max_ht,qm.fhd.can_max_ht) #fails linearity test                    
summary(rm.fhd.can_max_ht) #passes heteroskedasticity test

summary(lm.fhd.can_max_ht) #exponent = 0.33

## ENF ---------------------------------------------------------------------------------------

## here, moch is the canopy height variable --------------------------------------------------

logdat.enf<-logdat[logdat$pft=="ENF",]

#rugosity
lm.rugosity.moch<-lm(rugosity ~ moch, data=logdat.enf)
qm.rugosity.moch<-lm(rugosity ~ moch + I(moch^2), data=logdat.enf)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ logdat.enf$moch)

anova(lm.rugosity.moch,qm.rugosity.moch) #passes linearity test                    
summary(rm.rugosity.moch) #passes heteroskedasticity test

summary(lm.rugosity.moch) #exponent = 1.95

#enl
lm.enl.moch<-lm(enl ~ moch, data=logdat.enf)
qm.enl.moch<-lm(enl ~ moch + I(moch^2), data=logdat.enf)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ logdat.enf$moch)

anova(lm.enl.moch,qm.enl.moch) #passes linearity test                    
summary(rm.enl.moch) #fails heteroskedasticity test

#fhd
lm.fhd.moch<-lm(fhd ~ moch, data=logdat.enf)
qm.fhd.moch<-lm(fhd ~ moch + I(moch^2), data=logdat.enf)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ logdat.enf$moch)

anova(lm.fhd.moch,qm.fhd.moch) #passes linearity test                    
summary(rm.fhd.moch) #narrowly fails heteroskedasticity test

summary(lm.fhd.moch) #exponent = 0.18


## here, can_max_ht is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht, data=logdat.enf)
qm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht + I(can_max_ht^2), data=logdat.enf)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ logdat.enf$can_max_ht)

anova(lm.rugosity.can_max_ht,qm.rugosity.can_max_ht) #passes linearity test         
summary(rm.rugosity.can_max_ht) #passes heteroskedasticity test

summary(lm.rugosity.can_max_ht)

#enl
lm.enl.can_max_ht<-lm(enl ~ can_max_ht, data=logdat.enf)
qm.enl.can_max_ht<-lm(enl ~ can_max_ht + I(can_max_ht^2), data=logdat.enf)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ logdat.enf$can_max_ht)

anova(lm.enl.can_max_ht,qm.enl.can_max_ht) #passes linearity test                    
summary(rm.enl.can_max_ht) #fails heteroskedasticity test


#fhd
lm.fhd.can_max_ht<-lm(fhd ~ can_max_ht, data=logdat.enf)
qm.fhd.can_max_ht<-lm(fhd ~ can_max_ht + I(can_max_ht^2), data=logdat.enf)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ logdat.enf$can_max_ht)

anova(lm.fhd.can_max_ht,qm.fhd.can_max_ht) #passes linearity test                    
summary(rm.fhd.can_max_ht) #passes heteroskedasticity test

summary(lm.fhd.can_max_ht) #exponent = 0.32


## MF ---------------------------------------------------------------------------

## here, moch is the canopy height variable --------------------------------------------------

logdat.mf<-logdat[logdat$pft=="MF",]

#rugosity
lm.rugosity.moch<-lm(rugosity ~ moch, data=logdat.mf)
qm.rugosity.moch<-lm(rugosity ~ moch + I(moch^2), data=logdat.mf)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ logdat.mf$moch)

anova(lm.rugosity.moch,qm.rugosity.moch) #passes linearity test                    
summary(rm.rugosity.moch) #fails heteroskedasticity test

summary(lm.rugosity.moch) #exponent = 1.3

#enl
lm.enl.moch<-lm(enl ~ moch, data=logdat.mf)
qm.enl.moch<-lm(enl ~ moch + I(moch^2), data=logdat.mf)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ logdat.mf$moch)

anova(lm.enl.moch,qm.enl.moch) #fails linearity test                    
summary(rm.enl.moch) #fails heteroskedasticity test

summary(lm.enl.moch) #exponent = 0.63

#fhd
lm.fhd.moch<-lm(fhd ~ moch, data=logdat.mf)
qm.fhd.moch<-lm(fhd ~ moch + I(moch^2), data=logdat.mf)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ logdat.mf$moch)

anova(lm.fhd.moch,qm.fhd.moch) #fails linearity test                    
summary(rm.fhd.moch) #fails heteroskedasticity test

summary(lm.fhd.moch) #exponent = 0.22

## here, can_max_ht is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht, data=logdat.mf)
qm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht + I(can_max_ht^2), data=logdat.mf)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ logdat.mf$can_max_ht)

anova(lm.rugosity.can_max_ht,qm.rugosity.can_max_ht) #passes linearity test         
summary(rm.rugosity.can_max_ht) #fails heteroskedasticity test

summary(lm.rugosity.can_max_ht) #exponent = 2.62


#enl
lm.enl.can_max_ht<-lm(enl ~ can_max_ht, data=logdat.mf)
qm.enl.can_max_ht<-lm(enl ~ can_max_ht + I(can_max_ht^2), data=logdat.mf)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ logdat.mf$can_max_ht)

anova(lm.enl.can_max_ht,qm.enl.can_max_ht) #passes linearity test                    
summary(rm.enl.can_max_ht) #passes heteroskedasticity test

summary(lm.enl.can_max_ht)


#fhd
lm.fhd.can_max_ht<-lm(fhd ~ can_max_ht, data=logdat.mf)
qm.fhd.can_max_ht<-lm(fhd ~ can_max_ht + I(can_max_ht^2), data=logdat.mf)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ logdat.mf$can_max_ht)

anova(lm.fhd.can_max_ht,qm.fhd.can_max_ht) #passes linearity test                    
summary(rm.fhd.can_max_ht) #passes heteroskedasticity test

summary(lm.fhd.can_max_ht) #exponent = 0.33










###################################################################################################
## THIS uses PLOT as the unit of measure
###################################################################################################

unique(dat$plotID)

numdat.agg<-aggregate(dat[,c(4,6:10)], by=list(dat$plotID), FUN="mean", na.rm=T)
pft.agg<-aggregate(pft~plotID, data=dat, FUN=function(x){x[1]})

dat.agg<-left_join(pft.agg, numdat.agg, by=c("plotID"="Group.1"))

logdat.agg<-dat.agg
logdat.agg$moch<-log10(dat.agg$moch)
logdat.agg$can_max_ht<-log10(dat.agg$can_max_ht)
logdat.agg$rugosity<-log10(dat.agg$rugosity)
logdat.agg$enl<-log10(dat.agg$enl)
logdat.agg$fhd<-log10(dat.agg$fhd)



## here, moch is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.moch<-lm(rugosity ~ moch, data=logdat.agg)
qm.rugosity.moch<-lm(rugosity ~ moch + I(moch^2), data=logdat.agg)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ logdat.agg$moch)

anova(lm.rugosity.moch,qm.rugosity.moch) #passes linearity test                    
summary(rm.rugosity.moch) #fails heteroskedasticity test


#enl
lm.enl.moch<-lm(enl ~ moch, data=logdat.agg)
qm.enl.moch<-lm(enl ~ moch + I(moch^2), data=logdat.agg)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ logdat.agg$moch)

anova(lm.enl.moch,qm.enl.moch) #fails linearity test                    
summary(rm.enl.moch) #fails heteroskedasticity test

#fhd
lm.fhd.moch<-lm(fhd ~ moch, data=logdat.agg)
qm.fhd.moch<-lm(fhd ~ moch + I(moch^2), data=logdat.agg)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ logdat.agg$moch)

anova(lm.fhd.moch,qm.fhd.moch) #passes linearity test                    
summary(rm.fhd.moch) #passes heteroskedasticity test

summary(lm.fhd.moch) #exponent = 0.27; significantly different than 1
plot(logdat.agg$moch, logdat.agg$fhd, pch=16)

## here, can_max_ht is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht, data=logdat.agg)
qm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht + I(can_max_ht^2), data=logdat.agg)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ logdat.agg$can_max_ht)

anova(lm.rugosity.can_max_ht,qm.rugosity.can_max_ht) #fails linearity test                    
summary(rm.rugosity.can_max_ht) #fails heteroskedasticity test

#enl
lm.enl.can_max_ht<-lm(enl ~ can_max_ht, data=logdat.agg)
qm.enl.can_max_ht<-lm(enl ~ can_max_ht + I(can_max_ht^2), data=logdat.agg)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ logdat.agg$can_max_ht)

anova(lm.enl.can_max_ht,qm.enl.can_max_ht) #passes linearity test                    
summary(rm.enl.can_max_ht) #fails heteroskedasticity test

#fhd
lm.fhd.can_max_ht<-lm(fhd ~ can_max_ht, data=logdat.agg)
qm.fhd.can_max_ht<-lm(fhd ~ can_max_ht + I(can_max_ht^2), data=logdat.agg)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ logdat.agg$can_max_ht)

anova(lm.fhd.can_max_ht,qm.fhd.can_max_ht) #fails linearity test                    
summary(rm.fhd.can_max_ht) #passes heteroskedasticity test


## --------------------------------------------------------------------------
## Separate out plant functional types
# ---------------------------------------------------------------------------

## BDF ---------------------------------------------------------------------------------------

## here, moch is the canopy height variable --------------------------------------------------

logdat.agg.bdf<-logdat.agg[logdat.agg$pft=="DBF",]

#rugosity
lm.rugosity.moch<-lm(rugosity ~ moch, data=logdat.agg.bdf)
qm.rugosity.moch<-lm(rugosity ~ moch + I(moch^2), data=logdat.agg.bdf)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ logdat.agg.bdf$moch)

anova(lm.rugosity.moch,qm.rugosity.moch) #passes linearity test                    
summary(rm.rugosity.moch) #fails heteroskedasticity test

summary(lm.rugosity.moch) #exponent = 1.44

#enl
lm.enl.moch<-lm(enl ~ moch, data=logdat.agg.bdf)
qm.enl.moch<-lm(enl ~ moch + I(moch^2), data=logdat.agg.bdf)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ logdat.agg.bdf$moch)

anova(lm.enl.moch,qm.enl.moch) #passes linearity test                    
summary(rm.enl.moch) #passes heteroskedasticity test

summary(lm.enl.moch)

#fhd
lm.fhd.moch<-lm(fhd ~ moch, data=logdat.agg.bdf)
qm.fhd.moch<-lm(fhd ~ moch + I(moch^2), data=logdat.agg.bdf)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ logdat.agg.bdf$moch)

anova(lm.fhd.moch,qm.fhd.moch) #fails linearity test                    
summary(rm.fhd.moch) #passes heteroskedasticity test


## here, can_max_ht is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht, data=logdat.agg.bdf)
qm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht + I(can_max_ht^2), data=logdat.agg.bdf)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ logdat.agg.bdf$can_max_ht)

anova(lm.rugosity.can_max_ht,qm.rugosity.can_max_ht) #passes linearity test         
summary(rm.rugosity.can_max_ht) #passes heteroskedasticity test

summary(lm.rugosity.can_max_ht) #exponent = 2.54

#enl
lm.enl.can_max_ht<-lm(enl ~ can_max_ht, data=logdat.agg.bdf)
qm.enl.can_max_ht<-lm(enl ~ can_max_ht + I(can_max_ht^2), data=logdat.agg.bdf)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ logdat.agg.bdf$can_max_ht)

anova(lm.enl.can_max_ht,qm.enl.can_max_ht) #passes linearity test                    
summary(rm.enl.can_max_ht) # fails heteroskedasticity test

summary(lm.enl.can_max_ht)# exponent = 0.93, not different from 1

#fhd
lm.fhd.can_max_ht<-lm(fhd ~ can_max_ht, data=logdat.agg.bdf)
qm.fhd.can_max_ht<-lm(fhd ~ can_max_ht + I(can_max_ht^2), data=logdat.agg.bdf)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ logdat.agg.bdf$can_max_ht)

anova(lm.fhd.can_max_ht,qm.fhd.can_max_ht) #fails linearity test                    
summary(rm.fhd.can_max_ht) #passes heteroskedasticity test

summary(lm.fhd.can_max_ht) #exponent = 0.33

## ENF ---------------------------------------------------------------------------------------

## here, moch is the canopy height variable --------------------------------------------------

logdat.agg.enf<-logdat.agg[logdat.agg$pft=="ENF",]

#rugosity
lm.rugosity.moch<-lm(rugosity ~ moch, data=logdat.agg.enf)
qm.rugosity.moch<-lm(rugosity ~ moch + I(moch^2), data=logdat.agg.enf)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ logdat.agg.enf$moch)

anova(lm.rugosity.moch,qm.rugosity.moch) #passes linearity test                    
summary(rm.rugosity.moch) #passes heteroskedasticity test

summary(lm.rugosity.moch) #exponent = 1.95

#enl
lm.enl.moch<-lm(enl ~ moch, data=logdat.agg.enf)
qm.enl.moch<-lm(enl ~ moch + I(moch^2), data=logdat.agg.enf)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ logdat.agg.enf$moch)

anova(lm.enl.moch,qm.enl.moch) #passes linearity test                    
summary(rm.enl.moch) #fails heteroskedasticity test

#fhd
lm.fhd.moch<-lm(fhd ~ moch, data=logdat.agg.enf)
qm.fhd.moch<-lm(fhd ~ moch + I(moch^2), data=logdat.agg.enf)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ logdat.agg.enf$moch)

anova(lm.fhd.moch,qm.fhd.moch) #passes linearity test                    
summary(rm.fhd.moch) #narrowly fails heteroskedasticity test

summary(lm.fhd.moch) #exponent = 0.18


## here, can_max_ht is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht, data=logdat.agg.enf)
qm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht + I(can_max_ht^2), data=logdat.agg.enf)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ logdat.agg.enf$can_max_ht)

anova(lm.rugosity.can_max_ht,qm.rugosity.can_max_ht) #passes linearity test         
summary(rm.rugosity.can_max_ht) #passes heteroskedasticity test

summary(lm.rugosity.can_max_ht)

#enl
lm.enl.can_max_ht<-lm(enl ~ can_max_ht, data=logdat.agg.enf)
qm.enl.can_max_ht<-lm(enl ~ can_max_ht + I(can_max_ht^2), data=logdat.agg.enf)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ logdat.agg.enf$can_max_ht)

anova(lm.enl.can_max_ht,qm.enl.can_max_ht) #passes linearity test                    
summary(rm.enl.can_max_ht) #passes heteroskedasticity test

summary(lm.enl.can_max_ht)

#fhd
lm.fhd.can_max_ht<-lm(fhd ~ can_max_ht, data=logdat.agg.enf)
qm.fhd.can_max_ht<-lm(fhd ~ can_max_ht + I(can_max_ht^2), data=logdat.agg.enf)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ logdat.agg.enf$can_max_ht)

anova(lm.fhd.can_max_ht,qm.fhd.can_max_ht) #passes linearity test                    
summary(rm.fhd.can_max_ht) #passes heteroskedasticity test

summary(lm.fhd.can_max_ht) #exponent = 0.32


## MF ---------------------------------------------------------------------------

## here, moch is the canopy height variable --------------------------------------------------

logdat.agg.mf<-logdat.agg[logdat.agg$pft=="MF",]

#rugosity
lm.rugosity.moch<-lm(rugosity ~ moch, data=logdat.agg.mf)
qm.rugosity.moch<-lm(rugosity ~ moch + I(moch^2), data=logdat.agg.mf)

rs.rugosity.moch<-abs(residuals(lm.rugosity.moch))

rm.rugosity.moch<-lm(rs.rugosity.moch ~ logdat.agg.mf$moch)

anova(lm.rugosity.moch,qm.rugosity.moch) #passes linearity test                    
summary(rm.rugosity.moch) #fails heteroskedasticity test

summary(lm.rugosity.moch) #exponent = 1.3

#enl
lm.enl.moch<-lm(enl ~ moch, data=logdat.agg.mf)
qm.enl.moch<-lm(enl ~ moch + I(moch^2), data=logdat.agg.mf)

rs.enl.moch<-abs(residuals(lm.enl.moch))

rm.enl.moch<-lm(rs.enl.moch ~ logdat.agg.mf$moch)

anova(lm.enl.moch,qm.enl.moch) #fails linearity test                    
summary(rm.enl.moch) #fails heteroskedasticity test

summary(lm.enl.moch) #exponent = 0.63

#fhd
lm.fhd.moch<-lm(fhd ~ moch, data=logdat.agg.mf)
qm.fhd.moch<-lm(fhd ~ moch + I(moch^2), data=logdat.agg.mf)

rs.fhd.moch<-abs(residuals(lm.fhd.moch))

rm.fhd.moch<-lm(rs.fhd.moch ~ logdat.agg.mf$moch)

anova(lm.fhd.moch,qm.fhd.moch) #fails linearity test                    
summary(rm.fhd.moch) #fails heteroskedasticity test

summary(lm.fhd.moch) #exponent = 0.22

## here, can_max_ht is the canopy height variable --------------------------------------------------

#rugosity
lm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht, data=logdat.agg.mf)
qm.rugosity.can_max_ht<-lm(rugosity ~ can_max_ht + I(can_max_ht^2), data=logdat.agg.mf)

rs.rugosity.can_max_ht<-abs(residuals(lm.rugosity.can_max_ht))

rm.rugosity.can_max_ht<-lm(rs.rugosity.can_max_ht ~ logdat.agg.mf$can_max_ht)

anova(lm.rugosity.can_max_ht,qm.rugosity.can_max_ht) #passes linearity test         
summary(rm.rugosity.can_max_ht) #fails heteroskedasticity test

summary(lm.rugosity.can_max_ht) #exponent = 2.62


#enl
lm.enl.can_max_ht<-lm(enl ~ can_max_ht, data=logdat.agg.mf)
qm.enl.can_max_ht<-lm(enl ~ can_max_ht + I(can_max_ht^2), data=logdat.agg.mf)

rs.enl.can_max_ht<-abs(residuals(lm.enl.can_max_ht))

rm.enl.can_max_ht<-lm(rs.enl.can_max_ht ~ logdat.agg.mf$can_max_ht)

anova(lm.enl.can_max_ht,qm.enl.can_max_ht) #passes linearity test                    
summary(rm.enl.can_max_ht) #passes heteroskedasticity test

summary(lm.enl.can_max_ht)


#fhd
lm.fhd.can_max_ht<-lm(fhd ~ can_max_ht, data=logdat.agg.mf)
qm.fhd.can_max_ht<-lm(fhd ~ can_max_ht + I(can_max_ht^2), data=logdat.agg.mf)

rs.fhd.can_max_ht<-abs(residuals(lm.fhd.can_max_ht))

rm.fhd.can_max_ht<-lm(rs.fhd.can_max_ht ~ logdat.agg.mf$can_max_ht)

anova(lm.fhd.can_max_ht,qm.fhd.can_max_ht) #passes linearity test                    
summary(rm.fhd.can_max_ht) #passes heteroskedasticity test

summary(lm.fhd.can_max_ht) #exponent = 0.33

###################################################################################################
## Taylor's Power Law
###################################################################################################

canht.mn<-aggregate(can_max_ht~plotID, data=dat, FUN="mean", na.rm=T)
canht.sd<-aggregate(can_max_ht~plotID, data=dat, FUN="sd", na.rm=T)
colnames(canht.sd)<-c("plotID","sd_max_can_ht")


tpl.dat<-left_join(pft.agg, canht.mn)
tpl.dat<-left_join(tpl.dat, canht.sd, by=c("plotID"="plotID"))
tpl.dat<-tpl.dat[complete.cases(tpl.dat),]
tpl.dat<-tpl.dat[tpl.dat$sd_max_can_ht!=0,]

table(tpl.dat$pft)

tpl.dat$can_max_ht<-log(tpl.dat$can_max_ht)
tpl.dat$sd_max_can_ht<-log(tpl.dat$sd_max_can_ht)


lm.tpl<-lm(sd_max_can_ht~can_max_ht, data=tpl.dat)
qm.tpl<-lm(sd_max_can_ht~can_max_ht + I(can_max_ht^2), data=tpl.dat)

anova(lm.tpl, qm.tpl)

res.tpl<-abs(residuals(lm.tpl))

rm.tpl<-lm(res.tpl~tpl.dat$can_max_ht)
summary(rm.tpl)

summary(lm.tpl) #no significant relationship
