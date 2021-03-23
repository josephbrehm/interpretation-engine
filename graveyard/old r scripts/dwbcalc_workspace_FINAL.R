#### to do:
## add output for "what is limiting"
## run it on raster

### 0 load packages, define workspace ####
require(tidyverse) 
require(data.tree)
require(XML)
require(plyr)
require(soilDB)
require(digest)
require(ggplot2)

setwd("E:/NMSU/interp-engine-personal/Engine Guts/DWB")

### 1 load data ####
#load("indata.dwb.nasis.rda") ### created in the properties from nasis script
## big testing data set
load("indata.dwb.nasis.big1.rda")
load('../cached-NASIS-data.Rda')
#dbcrosswalk <- read.csv("..//..//dbcrosswalk//dbcrosswalk_inner_AK-AZ-CT-NE-UT-NM.csv")
dbcrosswalk <- read.csv("..//..//dbcrosswalk//dbcrosswalk_inner_UT.csv")

# rename indata columns
indata.check <-
  indata.dwb.nasis %>%
  dplyr::rename(
    depbrockhard = `DEPTH TO BEDROCK HARD, BELOW O HORIZON`,
    fragvol_wmn = `FRAGMENTS > 75MM WEIGHTED AVE. IN DEPTH 0-100CM`,
    permdepth = `DEPTH TO PERMAFROST`,
    floodfreq = `FLOODING FREQUENCY (Maximum Frequency)`,
    depbrocksoft = `DEPTH TO BEDROCK SOFT, BELOW O HORIZON`,
    lep = `LEP WTD_AVG 25-150cm OR ABOVE RESTRICTION, BELOW O HORIZON`,
    slope_r = `SLOPE`,
    depcementthick = `DEPTH TO CEMENTED PAN THICK, BELOW O HORIZON`,
    restricthardness = `RESTRICTIVE FEATURE HARDNESS`,
    depcementthin = `DEPTH TO CEMENTED PAN THIN, BELOW O HORIZON`,
    totalsub_r = `SUBSIDENCE TOTAL`,
    wt = `HIGH WATER TABLE DEPTH MINIMUM`,
    gypsum = `SUBSIDENCE DUE TO GYPSUM, REV`,
    bottomtexture = `UNIFIED BOTTOM LAYER`,
    texmod = `USDA TEXTURE MODIFIER`,
    texinlieu = `USDA TEXTURE (IN-LIEU-OF)`,
    ponddur = `PONDING DURATION`,
    pondfreq = `PONDING FREQUENCY`,
    unstablefill = `COMPONENT LOCAL PHASE UNSTABLE FILL`,
    impaction = `COMPONENT LOCAL PHASE IMPACTED`,
    drainageclass = `DRAINAGE CLASS IS NOT SUBAQUEOUS`,
    refdwb = `interplrc`
  ) %>%
  arrange(
    coiid
  )

### 2 load functions ####
# nrcs functions, edited version
source('../../Functions/local-functions - edited.R')

# evaluation by the eid code & dwb function
#source('../../Functions/evalbyeid.R')
source('../../Functions/dwb_calc.R')


### 3 dwb function ####
errcheck <- dwb_calc(indata = indata.check) %>% filter(refdwb != "Not rated")
head(errcheck$dwb)
summary(as.factor(errcheck$dwb))

### 4 validate ####
source("../../Functions/cm_refactor.R")

cm_refactor(pred = errcheck$dwb,
            ref = errcheck$refdwb)[2]
cm_refactor(pred = errcheck$dwb,
            ref = errcheck$refdwb)[[3]][1] # 97% as of 12-11 # 96.8 on 12/16

### 5 detailed validation ####
### do the fuzz scores match? ### yes. but with noticeable outliers
#plot(errcheck$interplr ~ errcheck$maxfuzz)
summary(lm(data = errcheck, interplr ~ maxfuzz))[[8]] ## r2 = .98 as of 12-11

### check each col
# 1 permafrost
ggplot(data = errcheck,
       aes(x = as.numeric(permdepth),
           y = fuzzpermdepth)) + 
  geom_point(aes(color = fuzzpermdepth == 1 | fuzzpermdepth == 0)) + geom_vline(xintercept = 50) + geom_vline(xintercept = 100)

# 2 ponding
summary(as.factor(errcheck[errcheck$fuzzpond == 1,]$ponddur)) # can't do a quick check; na's allowed if pondfreq case works
summary(as.factor(errcheck[errcheck$fuzzpond == 1,]$pondfreq)) # can't do a quick check; na's allowed if pondfreq case works

summary(as.factor(errcheck[errcheck$fuzzpond == 0,]$ponddur)) # all none or na
summary(as.factor(errcheck[errcheck$fuzzpond == 0,]$pondfreq)) # all none or na

# 3 slope
ggplot(data = errcheck,
       aes(x = as.numeric(slope_r),
           y = fuzzslope)) +
  geom_point(aes(color = fuzzslope == 1 | fuzzslope == 0)) +
  geom_vline(xintercept = 8) + geom_vline(xintercept = 15)

# 4 subsidence
ggplot(data = errcheck,
       aes(x = as.numeric(totalsub_r),
           y = fuzzsubsidence)) +
  geom_point(aes(color = fuzzsubsidence == 1 | fuzzsubsidence == 0)) +
  geom_vline(xintercept = 30)

# 5 flooding
summary(as.factor(errcheck[errcheck$fuzzfloodlim == 1,]$floodfreq)) # no none
summary(as.factor(errcheck[errcheck$fuzzfloodlim == 0,]$floodfreq)) # only none or na

# 6 water table #### ERROR IN ASYMPTOTES
ggplot(data = errcheck,
       aes(x = as.numeric(wt),
           y = fuzzwt)) +
  geom_point(aes(color = fuzzwt == 1 | fuzzwt == 0)) +
  geom_vline(xintercept = 75) + geom_vline(xintercept = 182)
# true error in asymptotes

# 7 shrink swell
ggplot(data = errcheck,
       aes(x = as.numeric(lep),
           y = fuzzshrinkswell)) +
  geom_point(aes(color = fuzzshrinkswell == 1 | fuzzshrinkswell == 0)) +
  geom_vline(xintercept = 6) + geom_vline(xintercept = 3)

# 8 om
summary(as.factor(errcheck[errcheck$fuzzomlim == 1,]$bottomtexture)) # only pt / ol / oh
summary(as.factor(errcheck[errcheck$fuzzomlim == 0,]$bottomtexture)) # none of the above

# 9 depth to hard bedrock
ggplot(data = errcheck,
       aes(x = as.numeric(depbrockhard),
           y = fuzzdepbrockhard)) +
  geom_point(aes(color = fuzzdepbrockhard == 1 | fuzzdepbrockhard == 0)) +
  geom_vline(xintercept = 100) + geom_vline(xintercept = 150)

# 10 depth to soft bedrock
ggplot(data = errcheck,
       aes(x = as.numeric(depbrocksoft),
           y = fuzzdepbrocksoft)) +
  geom_point(aes(color = fuzzdepbrocksoft == 1 | fuzzdepbrocksoft == 0)) +
  geom_vline(xintercept = 100) + geom_vline(xintercept = 50)

# 11 large stones
ggplot(data = errcheck,
       aes(x = as.numeric(fragvol_wmn),
           y = fuzzlgstone)) +
  geom_point(aes(color = fuzzlgstone == 1 | fuzzlgstone == 0)) +
  geom_vline(xintercept = 25) + geom_vline(xintercept = 50)

# 12 thick cement pan
ggplot(data = errcheck,
       aes(x = as.numeric(depcementthick),
           y = fuzzdepcementthick)) +
  geom_point(aes(color = fuzzdepcementthick == 1 | fuzzdepcementthick == 0)) +
  geom_vline(xintercept = 100) + geom_vline(xintercept = 150)

# 13 thin cement pan
ggplot(data = errcheck,
       aes(x = as.numeric(depcementthin),
           y = fuzzdepcementthin)) +
  geom_point(aes(color = fuzzdepcementthin == 1 | fuzzdepcementthin == 0)) +
  geom_vline(xintercept = 100) + geom_vline(xintercept = 50)

# 14 unstable fill
summary(as.factor(errcheck[errcheck$fuzzunstable == 1,]$unstablefill)) ### should all be "unstable fill" or 1
summary(as.factor(errcheck[errcheck$fuzzunstable == 0,]$unstablefill)) ### no unstable fill or 1

# 15 gypsum
ggplot(data = errcheck,
       aes(x = as.numeric(gypsum),
           y = fuzzgypsum)) +
  geom_point(aes(color = fuzzgypsum == 1 | fuzzgypsum == 0)) +
  geom_vline(xintercept = 3) + geom_vline(xintercept = 18)

# 16 impaction
summary(as.factor(errcheck[errcheck$fuzzimpaction == 1,]$impaction)) ### not enough data ### should all be "impacted soil" or 1
summary(as.factor(errcheck[errcheck$fuzzimpaction == 0,]$impaction)) ### not enough data ### should all be 0; no impacted soil

# 17 drainage
summary(as.factor(errcheck[errcheck$fuzzdrainage == 1,]$drainageclass)) ### not enough data ### should all be 0
summary(as.factor(errcheck[errcheck$fuzzdrainage == 0,]$drainageclass)) ### not enough data ### should all be 1 or na