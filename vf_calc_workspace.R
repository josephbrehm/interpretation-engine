### 0 setup ####
require(plyr)
require(tidyverse) 
require(data.tree)
require(XML)
require(soilDB)
require(digest)
require(raster)

setwd("E:/NMSU/interp-engine-personal/Engine Guts/VF")
load('../cached-NASIS-data.Rda')
source('../../Functions/local-functions - edited.R')
source('../../Functions/evalbyeid.R')
# find it
rules[grepl("saprophyte", rules$rulename, ignore.case = T),"rulename"]

dt <- initRuleset('Soil Habitat for Saprophyte Stage of Coccidioides')
options(width=300)
printtree <- print(dt, 'Type', 'Value', 'eval_refid', limit=NULL)

dt2 <- initRuleset('ENG - Dwellings With Basements')
options(width=300)
printtree2 <- print(dt2, 'Type', 'Value', 'eval_refid', limit=NULL)

printtree

### testing function

get_eval <- function(e){
  #e = 43000
  d <- subset(evals, evaliid == e)
  f <- extractEvalCurve(d)
  l <- xmlChunkParse(d$eval)
  type <- d$evaluationtype
  
  lims <- c(d$propmin, d$propmax)
  xvals <- unlist(l$DomainPoints)
  yvals <- unlist(l$RangePoints)
  
  
  outlist <- list(e, d, f, type, lims, xvals, yvals)
  names(outlist) <- c(
    "evaliid", 
    "evalrow",
    "approxfun",
    "evaltype", 
    "xlimits",
    "domainpts",
    "rangepts"
  )
  return(outlist)
}

### 1 get data ####
#### fix your file paths, this is a mess

indata <- read.csv("../../TEST FULL WORKFLOW/Input/VFpropertyTestData-UT.csv") %>%
  mutate(mukey = as.character(mukey))
### 2 calc function ####

# 
# vf_calc <- function(indata, 
#                     xeric = "auto" # force xeric or nonxeric function, or let it choose for each data row/pixel depending on an input raster (accepts "auto" / "xeric" / "nonxeric")
# ){ 
#   if(!(xeric %in% c("auto", "xeric", "nonxeric"))){
#     print("Unknown input for xeric parameter, which determines whether to process data as xeric or nonxeric conditions")
#     print("Accepted input is 'auto', 'xeric', or 'nonxeric'.")
#     print("Defaulting to auto. This requires data specifying conditions by data point, and will error if not provided")
#     xeric <- "auto"
#   }
#   
#   ### this works. cut it out for simplicity for now
#   rasterinput <- class(indata)[1] %in% c("RasterStack", "RasterBrick")
#   #print(rasterinput)
#   if(rasterinput) {
#     ### add a "require raster" here. and not elsewhere.
#     r.template <- indata[[1]] # save the first one as a template to create an out raster later
#     indata <- as.data.frame(indata)
#   }
#   
#   #### TESTING!!! ###
#   #indata <- testindata3
#   ###
#   
#   outdata <- indata %>% dplyr::select(cokey, coiid, comp_name, mukey, muiid, xeric)
#   
#   ## 1 - chemical subrule
#   outdata$fuzzsar <- evalbyeid(42999, indata$sar, sig.scale = 5) %>% replace_na(0)
#   outdata$fuzzec <- evalbyeid(43000, indata$ec) %>% replace_na(0)
#   outdata$fuzzgypsumcontent <- evalbyeid(42991, indata$gypsumcontent) ^ (0.5) %>% replace_na(0)
#   outdata$fuzzph <- evalbyeid(42985, indata$ph, sig.scale = 0.125) %>% replace_na(0)
#   
#   outdata$fuzzchem <- 
#     do.call(pmax, c(outdata[,c('fuzzsar', 'fuzzec', 'fuzzgypsumcontent', 'fuzzph')], na.rm = T))
#   
#   ## 2 - climatic subrule
#   ## allow for forced xeric or nonxeric conditions
#   if(!("xeric"  %in% colnames(indata))) indata$xeric <- NA
#   if(xeric == "xeric") {indata$xeric <- 1}
#   if(xeric == "nonxeric") {indata$xeric <- 0}
#   
#   
#   #### redo this format. ##
#   ### xeric as t/f/auto
#   ### three columns. xeric, nonxeric, correct answer
# 
#   ## identify which rows to treat as xeric/nonxeric
#   #xrows <- indata$xeric == 1 & !is.na(indata$xeric)
#   #nxrows <- indata$xeric == 0 & !is.na(indata$xeric)
#   #unkxrows <- is.na(indata$xeric) | !(indata$xeric %in% c(1,0))
#   
#   #sum(xrows) + sum(nxrows) + sum(unkxrows) - nrow(indata)
#   
#   ### 2a - precip
#   
#   ## these two to vectors because they are only with a subset of rows
#   #fuzzprecipX <- evalbyeid(42997, indata[xrows, "map"])
#   #fuzzprecipNX <- evalbyeid(42998, indata[nxrows, "map"])
#   
#   outdata$fuzzprecipX <- evalbyeid(42997, indata$map)
#   outdata$fuzzprecipNX <- evalbyeid(42998, indata$map)
#   
#   ### 2b - slope heating factor -- no change with xeric/nonxeric
#   
#   outdata$fuzzalbedo <- evalbyeid(43047, 1 - indata$albedo)
#   ## Data drawn from nasis web reports (which is all of it) rounds to integers, so meaningless for albedo.
#   ## for testing I used ssurgo data
#   ## nasis albedo is inverted from gssurgo?????? WHY????
#   
#   ### aspect eval runs off of a "factor" not an aspect, and the factor runs into the integer error.
#   ### it has to be recreated here because web export turns the 0-1 decimal into 0 or 1 integer.
#   ### and so that people can sub in 0-360 aspect data
#   
#   indata$aspectfactor = if_else( ## this is translated from cvir "VALLEY FEVER ASPECT FACTOR", property 35987
#     is.na(indata$aspect), 0,
#     if_else(
#       indata$aspect >= 0 & indata$aspect < 0, 0,
#       if_else(
#         indata$aspect >= 80 & indata$aspect < 280, -((indata$aspect-180)**2)/9000+1,
#         0 # if all false
#       )
#     )
#   )
#   
#   outdata$fuzzslopeheatload <- evalbyeid(43048, indata$slope)
#   outdata$fuzzaspect <- evalbyeid(43049, indata$aspectfactor)
#   outdata$fuzzheatingfactor <- 
#     outdata$fuzzalbedo *
#     outdata$fuzzslopeheatload *
#     outdata$fuzzaspect
#   
#   
#   
#   ### 2c - surface temp limiters
#   # cut the choppers. they're just ratios based on a real measure, air temp. And they dont work with the property extraction, because of the decimal rounding error
#   # also, why is 42996 used twice ## supposed to be 43051? ## 43051 doesnt exist. 
#   
#   # fuzzsurftempX <- evalbyeid(43050, indata[xrows, "xericairtempchopper"])
#   # fuzzsurftempNX <- evalbyeid(42996, indata[nxrows, "nonxericairtempchopper"])
#   
#   # fuzzsurftempX <- evalbyeid(43050, indata[xrows, "airtemp"] / 16)
#   # fuzzsurftempNX <- evalbyeid(42996, indata[nxrows, "airtemp"] / 18.5)
#   # 
#   # fuzzairtempX <- evalbyeid(42995, indata[xrows, "airtemp"])
#   # fuzzairtempNX <- evalbyeid(42996, indata[nxrows, "airtemp"])
#   # 
#   
#   outdata$airtempchopperX <- indata$airtemp / 16
#   outdata$airtempchopperNX <- indata$airtemp / 18.5
#   
#   # names in nasis vary for these ones
#   # xeric is Xeric Surface Temperature Limiter
#   # nonxeric is Non-Xeric Air Temperature Limiter, Valley Fever
#   # both use air temp choppers, based on 
#   
#   
#   
#   
#   outdata$fuzzsurftempX <- evalbyeid(43050, outdata$airtempchopperX)
#   outdata$fuzzsurftempNX <- evalbyeid(43051, outdata$airtempchopperNX) 
#   
#   outdata$fuzzairtempX <- evalbyeid(42995, indata$airtemp)
#   outdata$fuzzairtempNX <- evalbyeid(42996, indata$airtemp)
#   
#   ### combine all the x/nx rows together
#   
#   # outdata$fuzzprecip <- as.numeric(NA)
#   # outdata[xrows, "fuzzprecip"] <- fuzzprecipX
#   # outdata[nxrows, "fuzzprecip"] <- fuzzprecipNX
#   # 
#   # outdata$fuzzsurftemp <- as.numeric(NA)
#   # outdata[xrows, "fuzzsurftemp"] <- fuzzsurftempX  
#   # outdata[nxrows, "fuzzsurftemp"] <- fuzzsurftempNX  
#   # 
#   # outdata$fuzzairtemp <- as.numeric(NA)
#   # outdata[xrows, "fuzzairtemp"] <- fuzzairtempX  
#   # outdata[nxrows, "fuzzairtemp"] <- fuzzairtempNX  
# 
#   outdata$fuzzprecip <-
#     if_else(
#       indata$xeric == 1,
#       outdata$fuzzprecipX,
#       outdata$fuzzprecipNX
#     )
#   
#   outdata$fuzzsurftemp <-
#     if_else(
#       indata$xeric == 1,
#       outdata$fuzzsurftempX,
#       outdata$fuzzsurftempNX
#     )
#   
#   outdata$fuzzairtemp <-
#     if_else(
#       indata$xeric == 1,
#       outdata$fuzzairtempX,
#       outdata$fuzzairtempNX
#     )
#   
#   #### temporary thing for err check!!! delme!!!!
#   # outdata$fuzzsurftemp <- 0.861
#   
#   ###
#   
#   
#   outdata$fuzzclimate <- 
#     (outdata$fuzzheatingfactor * outdata$fuzzsurftemp + outdata$fuzzairtemp) * outdata$fuzzprecip
#   
#   ## 3 - WRD 0-30cm 5-11%
#   outdata$fuzzwrd <- evalbyeid(42987, indata$wrd, sig.scale = 2)
#   
#   ## 4 - water and spore gathering surface
#   outdata$fuzzwatergatheringsurface <- evalbyeid(42988, indata$watergatheringsurface)
#   
#   ## 5 - organic carbon
#   outdata$fuzzom <- evalbyeid(42990, indata$om)
#   
#   ## 6 - near surface saturation
#   outdata$fuzzsaturation <- evalbyeid(63800, indata$saturationmonths)
#   outdata$fuzzflood <- evalbyeid(63801, indata$floodmonths) #### this is not used?
#   
#   outdata$fuzzsurfsat <- 
#     do.call(pmin, c(outdata[,c('fuzzsaturation', 'fuzzflood')], na.rm = T))
#   
#   ### combine all the subrules
#   outdata$fuzzvf <- 
#     sqrt(
#       sqrt(outdata$fuzzwrd) * 
#         sqrt(outdata$fuzzwatergatheringsurface) * 
#         sqrt(outdata$fuzzom) * 
#         sqrt(outdata$fuzzsurfsat) *
#         outdata$fuzzclimate * 
#         outdata$fuzzchem) /
#     0.95
#   
#   # in rare cases, this can be more than 1 (highest val seen in testing was 1.026)
#   # clamp it, for data quality & appearance (this should not have meaningful changes. If it does, there is an error here somewhere)
#   outdata[outdata$fuzzvf > 1 & !is.na(outdata$fuzzvf), "fuzzvf"] <- 1
#   
#   classbreaks <- c(0, 0.1, 0.2, 0.5, 0.8, 1)
#   classlabels <- c("Not suitable", "Somewhat suitable", "Moderately suitable", "Suitable", "Highly suitable")
#   
#   outdata$vf <- base::cut(outdata$fuzzvf, breaks = classbreaks, labels = classlabels, right = TRUE, include.lowest = T)
#   
#   outdata <- 
#     outdata %>%
#     mutate(cokey = as.character(cokey),
#            coiid = as.character(coiid),
#            mukey = as.character(mukey))
#   
#   ## all fuzzy values are returned, as is the column for the maximum fuzzy score, and the final classification
#   ## if the input was a raster stack or brick, the output will be a small 2 layer brick instead of the df
#   if(rasterinput){
#     vf <- r.template %>%
#       setValues(factor(outdata$vf),
#                 levels = rev(classlabels))
#     fuzzdwb <- r.template %>%
#       setValues(outdata$fuzzvf)
#     
#     outdatabrk <- brick(vf, fuzzvf)
#     names(outdata) <- c("vf", "fuzzvf")
#   }
#   
#   return(outdata)
# }

source("../../Functions/vf_calc.r")

outdata <- vf_calc(indata, doxeric = "auto")
outdata$cokey <- indata$cokey
outdata$mukey <- indata$mukey
summary(as.factor(outdata$vf))
summary(outdata$fuzzvf)


#### VALIDATION #####
### cut all this when validated
valdata <- read.csv("../../cocci_2020_export_max_aggr.csv")

classbreaks <- c(0, 0.1, 0.2, 0.5, 0.8, 1)
classlabels <- c("Not suitable", "Somewhat suitable", "Moderately suitable", "Suitable", "Highly suitable")

valdata$refvf <- base::cut(valdata$Max_HSI, breaks = classbreaks, labels = classlabels, right = TRUE, include.lowest = T)
valdata$Mukey <- as.character(valdata$Mukey)


errcheck <- 
  outdata %>%
  group_by(mukey) %>%
  arrange(-fuzzvf, mukey) %>% ### data from bob dobos is the HIGHEST suitability per map unit
  summarize_all(first) %>%
  mutate(cokey = as.character(cokey)) %>%
  left_join(valdata,
            by = c("mukey" = "Mukey")) %>%
  left_join(indata %>% dplyr::select(cokey, mukey, xeric) %>% mutate(cokey = as.character(cokey), mukey = as.character(mukey)) # add the xeric tag
  )



errcheck[1:20,c("cokey", "mukey", "fuzzvf", "Max_HSI")]



#### VALIDATE ON DATA FRAME -- does not validate.
source("../../Functions/cm_refactor.R")

cm_refactor(refvar = errcheck$refvf,
            predvar = errcheck$vf)

# 2.18 11am: acc 0.7352
# 2.18 1pm: acc 0.7381, after adding real albedo data
# Reference
# Reference
# Prediction            Highly suitable Moderately suitable Not suitable Somewhat suitable Suitable (Missing)
# Highly suitable                   0                   0            0                 0        0         0
# Moderately suitable               0                  14            1                 0        7         0
# Not suitable                      0                 313         3670               775       39       157
# Somewhat suitable                 8                  21            0                 2        6         0
# Suitable                          0                   0            0                 0        0         0
# (Missing)                         0                   0           44                 4        0       189

# 2.22 9am: acc 0.7577, after adding manual invert results thing to albedo portion
# Reference
# Prediction            Highly suitable Moderately suitable Not suitable Somewhat suitable Suitable (Missing)
# Highly suitable                   0                   0            0                 0        0         0
# Moderately suitable               0                  27            1                 2        4         0
# Not suitable                      0                 217         3653               671       26       155
# Somewhat suitable                 8                 100           17               104       17         2
# Suitable                          0                   4            0                 0        5         0
# (Missing)                         0                   0           44                 4        0       189
# 

# 2.24 9am: acc 0.7611, after fixing the final eqn
# Reference
# Prediction            Highly suitable Moderately suitable Not suitable Somewhat suitable Suitable (Missing)
# Highly suitable                   8                   5            0                 0        6         0
# Moderately suitable               0                 103           56               118       13         4
# Not suitable                      0                  15         3263               245        0        69
# Somewhat suitable                 0                 187          341               406        0        84
# Suitable                          0                  37           12                 9       28         1
# (Missing)                         0                   1           43                 3        5       188

# 2.26 8am: acc 0.7859, after fixing albedo
# Reference
# Prediction            Highly suitable Moderately suitable Not suitable Somewhat suitable Suitable (Missing)
# Highly suitable                   6                   3            0                 0       12         0
# Moderately suitable               0                 269           65               233        4         7
# Not suitable                      0                  13         3262               170        0        73
# Somewhat suitable                 0                  29          332               365        0        76
# Suitable                          0                  34           12                 9       35         1
# (Missing)                         2                   0           44                 4        1       189

# 3.2 12pm: acc 0.7863, after changing how xeric flag is used
# 3.3: acc 0.8278, after changing how xeric flag is assigned
# Reference
# Prediction            Highly suitable Moderately suitable Not suitable Somewhat suitable Suitable (Missing)
# Highly suitable                   8                   0            0                 0        6         0
# Moderately suitable               0                 249            4               113       22         3
# Not suitable                      0                  18         3450               234        1        84
# Somewhat suitable                 0                  72          217               430        3        70
# Suitable                          0                   9            0                 0       20         0
# (Missing)                         0                   0           44                 4        0       189

# 3.4 10am: acc .8335 after fixing which eval wrd uses and tuning the sigmoid functions
# 3.9 acc .8343 after adding sqrt to the water gathering surface function

### just xerics: #### CANT DO NON XERICS WITH THIS DATASET! NEED AN AREA WITH MORE DATAPOINTS

xerrcheck <- errcheck[errcheck$xeric ==  T,]
nxerrcheck <- errcheck[errcheck$xeric ==  F,]

cm_refactor(refvar = xerrcheck$refvf,
            predvar = xerrcheck$vf)

cm_refactor(refvar = nxerrcheck$refvf,
            predvar = nxerrcheck$vf)

# plot(Max_HSI ~ fuzzvf, data = errcheck %>%filter(!is.na(refvf), !is.na(vf)))
summary(lm(Max_HSI ~ fuzzvf, data = errcheck))


ggplot(data = errcheck %>% filter(!is.na(refvf), !is.na(vf)),
       aes(fuzzvf, Max_HSI)) + geom_point(aes(color = xeric))


## check fuzzy values for a single cokey
dbcrosswalk <- read.csv("E:/NMSU/interp-engine-personal/dbcrosswalk/dbcrosswalk_inner_UT.csv")
dbcrosswalk <- dbcrosswalk %>% mutate_all(as.character)

t.nasisjoin <- read.csv("E:/NMSU/interp-engine-personal/TEST FULL WORKFLOW/Input/VFpropertyTestDataNoRename-UT.csv") %>% 
  mutate(mukey = as.character(mukey),
         cokey = as.character(cokey),
         coiid = as.character(coiid))

### pick a cokey
errcheck %>% 
  filter(
    refvf != vf,
    !is.na(refvf),
    !is.na(vf)
  ) %>% dplyr::select(cokey, coiid, comp_name, refvf, vf, Max_HSI, fuzzvf) %>%
  mutate(
    fuzzdiff = fuzzvf - Max_HSI
  ) %>%
  arrange(
    -fuzzdiff
  ) %>%
  filter(
    fuzzdiff > 0.1
  )

### error summary, bezzant cokey 19640852 ####
testcokey1 <- "19640852"
testcoiid1 <- dbcrosswalk %>% filter(cokey == testcokey1) %>% dplyr::select(coiid) %>% as.character()
testfuzzies1 <- outdata %>% filter(cokey == testcokey1) %>% left_join(t.nasisjoin)
subset(errcheck, cokey == testcokey1) %>% dplyr::select(refvf, vf, Max_HSI, fuzzvf)

## format is mine / correct answers
####### fuzzies, now reordered to match nasis export
## overall
# 0.04009003 to .032
testfuzzies1$fuzzvf

## clim subrule
# 0.0164944 to 0.01
testfuzzies1$fuzzclimate

## Non-Xeric Air Temperature, Valley Fever
# 0 to 0
testfuzzies1$fuzzairtempNX

# heating factor / Slope Heating Factor Subrule for Valley Fever
### 0.0793 to 0.079
testfuzzies1$fuzzheatingfactor

# aspect / Slope Aspect Factor valley Fever
### 0.1 to 0.1
testfuzzies1$fuzzaspect

# albedo / albedo
# 0.793 to 0.793
### does require input data be inverted, no idea why, but the numbers match exactly so hooray
### look into this when subbing in dsm data
testfuzzies1$fuzzalbedo

# slope heat load / Slope Gradient Valley Fever Heat Loading
# 1 to 1
testfuzzies1$fuzzslopeheatload

# airtemp / Xeric Air Temperature, Valley Fever
### 0 to 0.523!!!!!!! 
testfuzzies1$fuzzairtempX

# surftempNX / Non-Xeric Air Temperature Limiter, Valley Fever ### nb that the nasis names for X/NX vary
### 0.2410175 to 0.147
testfuzzies1$fuzzsurftempNX

# surftemp X / Xeric Surface Temperature Limiter, Valley Fever
### .4 to .243
testfuzzies1$fuzzsurftempX

# precipNX / Non-Xeric Precipitation, Valley Fever
### .46 to .467
testfuzzies1$fuzzprecipNX

# precipX / Xeric Precipitation, Valley Fever
### 0.52 / 0.519
testfuzzies1$fuzzprecip

# chem overall / Chemical Subrule for Valley Fever
### 1 to 0.894. this is a max, both coming from gypsum
testfuzzies1$fuzzchem

# sar / SAR, WTD_AVG to 30cm: 
### 0 to 0, still untested
testfuzzies1$fuzzsar

# gypsum content / Gypsum 0 to 15% within 30cm:
### 0 to 0
testfuzzies1$fuzzgypsumcontent

# ph / Soil Reaction WTD_AVG 0-30cm
# 0.0179 to 0, after the sig scale change
testfuzzies1$fuzzph

# ec / Valley Fever EC WTD_AVG 0 to 30 cm, 8-16mmhos: 
### 0.1 to 0.1
testfuzzies1$fuzzec

# water gathering surface  / Water and Spore Gathering Surface
### 0.8 to 0.894
testfuzzies1$fuzzwatergatheringsurface

# om / Organic Carbon, kg/m2 to 30cm
### 0.96667 to 0.994
testfuzzies1$fuzzom

# wrd / WRD 0 to 30cm, 5 to 11 percent
### 1 to 1
testfuzzies1$fuzzwrd

# saturation / Valley Fever Near Surface Saturation
### 1 to 1
testfuzzies1$fuzzsaturation
# 
# 
# ### pick a new cokey to check, with differences in the output
# subset(errcheck, vf != refvf)
# 
# dbcrosswalk$coiid <- as.character(dbcrosswalk$coiid)
# dbcrosswalk$cokey <- as.character(dbcrosswalk$cokey)
# errcheck$coiid <- as.character(errcheck$coiid)
# errcheck$cokey <- as.character(errcheck$cokey)
# indata$coiid <- as.character(indata$coiid)
# indata$cokey <- as.character(indata$cokey)
# 
# errcheck %>% 
#   filter(vf != refvf) %>%
#   filter(vf == "Not suitable" & refvf == "Suitable") %>%
#   left_join(indata) %>% 
#   dplyr::select(refvf, vf, Max_HSI, fuzzvf, cokey, coiid, comp_name) %>%
#   arrange(-Max_HSI, -fuzzvf)

### error summary, modena cokey 19699228 ####
testcokey2 <- "19699228"
testcoiid2 <- dbcrosswalk %>% filter(cokey == testcokey2) %>% dplyr::select(coiid) %>% as.character()
testfuzzies2 <- outdata %>% filter(cokey == testcokey2) %>% left_join(t.nasisjoin)
testindata2 <- indata %>% filter(cokey == testcokey2)

errcheck %>% filter(cokey == testcokey2) %>% dplyr::select(Max_HSI, fuzzvf, xeric)

#### should be using the xeric rules...


## format is mine / correct answers
####### fuzzies, now reordered to match nasis export
## overall
# 0.2895686 to .81
testfuzzies2$fuzzvf

# but .7995 if forced to be xeric
vf_calc(testindata2, xeric = "xeric")$fuzzvf


## clim subrule
# 0.08843895 to .689
testfuzzies2$fuzzclimate

## Non-Xeric Air Temperature, Valley Fever
# 0 to 0
testfuzzies2$fuzzairtempNX

# heating factor / Slope Heating Factor Subrule for Valley Fever
### 0.127896 to 0.143
testfuzzies2$fuzzheatingfactor

# aspect / Slope Aspect Factor valley Fever
### 0.24 to 0.264
testfuzzies2$fuzzaspect

# albedo / albedo
# 0.73 to 0.73
testfuzzies2$fuzzalbedo

# slope heat load / Slope Gradient Valley Fever Heat Loading
# 0.73 to 0.74
testfuzzies2$fuzzslopeheatload

# airtemp / Xeric Air Temperature, Valley Fever
### 0.6 to 0.6!!!!!!! 
testfuzzies2$fuzzairtempX

# surftempNX / Non-Xeric Air Temperature Limiter, Valley Fever ### nb that the nasis names for X/NX vary
### 0.7081081 to 0.655
testfuzzies2$fuzzsurftempNX

# surftemp X / Xeric Surface Temperature Limiter, Valley Fever
### .85 to .861
testfuzzies2$fuzzsurftempX

# precipX / Xeric Precipitation, Valley Fever
### 0.9765333 / 0.954
testfuzzies2$fuzzprecip

# precipNX / Non-Xeric Precipitation, Valley Fever
### 0.9765333 to .976
testfuzzies2$fuzzprecipNX

# chem overall / Chemical Subrule for Valley Fever
### .982 to .92
testfuzzies2$fuzzchem

# sar / SAR, WTD_AVG to 30cm: 
### 0 to 0, still untested
testfuzzies2$fuzzsar

# gypsum content / Gypsum 0 to 15% within 30cm:
### 0 to 0
testfuzzies2$fuzzgypsumcontent

# ec / Valley Fever EC WTD_AVG 0 to 30 cm, 8-16mmhos: 
### 0.4333 to 0.479
testfuzzies2$fuzzec

# ph / Soil Reaction WTD_AVG 0-30cm
# 0.9820138 to 0.92
testfuzzies2$fuzzph

# water gathering surface  / Water and Spore Gathering Surface
### 0.9 to 0.949
testfuzzies2$fuzzwatergatheringsurface

# om / Organic Carbon, kg/m2 to 30cm
### 0.9333333 to 0.951
testfuzzies2$fuzzom

# wrd / WRD 0 to 30cm, 5 to 11 percent
### 0.9038553 to 0.968
testfuzzies2$fuzzwrd

# saturation / Valley Fever Near Surface Saturation
### 1 to 1
testfuzzies2$fuzzsaturation


### ok. so all the components are correct, and the final answer is way off




# ####### fuzzies
# ##### chem subrule
# # sar / SAR, WTD_AVG to 30cm: 
# testfuzzies2$fuzzsar
# ### 0 to 0, still untested
# 
# # ec / Valley Fever EC WTD_AVG 0 to 30 cm, 8-16mmhos: 
# testfuzzies2$fuzzec
# ### 0.4 to 0.48
# #### call it good
# 
# # gypsum content / Gypsum 0 to 15% within 30cm:
# ### 0 to 0
# ## still untested
# testfuzzies2$fuzzgypsumcontent
# 
# # ph / Soil Reaction WTD_AVG 0-30cm
# # .99 to .92, after the sig scale change
# testfuzzies2$fuzzph
# 
# # chem overall / Chemical Subrule for Valley Fever
# ### .99 to .92, ph err resolved
# testfuzzies2$fuzzchem
# 
# # albedo / albedo
# ### .73 to .73! linear evals are so nice and easy
# ### does require input data be inverted, no idea why, but the numbers match exactly so hooray
# testfuzzies2$fuzzalbedo
# 
# # slope / Valley Fever Heat Loading Slope
# ### 0.73 to 0.74
# testfuzzies2$fuzzslopeheatload
# 
# # aspect / Slope Aspect Factor valley Fever
# ## requires recalced slope aspect factor, due to decimal error
# ### 0.24 to 0.264
# testfuzzies2$fuzzaspect
# 
# # heating factor / Slope Heating Factor Subrule for Valley Fever
# ### 0.128 to 0.143
# testfuzzies2$fuzzheatingfactor
# 
# #### clim stuff all uses xeric rules
# testindata2$xeric
# 
# # precip / Xeric Precipitation, Valley Fever
# ### 0.951 / 0.954
# testfuzzies2$fuzzprecip
# 
# # surftemp / Xeric Surface Temperature Limiter, Valley Fever
# ### .7 to 0.861 ### somethings maybe up here, has a decimal rounding err and a confusing eid tag
# testfuzzies2$fuzzsurftemp
# 
# 
# # airtemp / Xeric Air Temperature, Valley Fever
# ### 0.6 to 0.6
# testfuzzies2$fuzzairtemp
# 
# # climate subrule / Climatic Subrule for Valley Fever
# ### 0.619 to 0.689
# testfuzzies2$fuzzclimate
# 
# # wrd / WRD 0 to 30cm, 5 to 11 percent
# ### 1 to 0.968
# testfuzzies2$fuzzwrd
# 
# # water gathering surface  / Water and Spore Gathering Surface
# ### 0.9 to 0.949
# testfuzzies2$fuzzwatergatheringsurface
# 
# # om / Organic Carbon, kg/m2 to 30cm
# ### 0.93 to 0.95
# testfuzzies2$fuzzom
# 
# # saturation / Valley Fever Near Surface Saturation
# ### 1 to 1
# testfuzzies2$fuzzsaturation
# 
# ### last things
# ## overall
# # 0.79 to 0.81
# testfuzzies2$fuzzvf
# 
# ## clim subrule
# # 0.619 to 0.689
# testfuzzies2$fuzzclimate
# 
# ## chem subrule
# # 0.99 to 0.92
# testfuzzies2$fuzzchem
# 
# ## final eqn, using values from nasis export
# sqrt(
#   sqrt(0.968) * # wrd
#     sqrt(0.949) * # wg surface
#     sqrt(0.951) * # om
#     sqrt(1) * # saturation
#     0.689 *  # clim subrule
#     0.92) / # chem subrule
#   0.95  # constant
# # 0.81 to 0.81!!!!!

### error summary, goblin family cokey 19722620 #####
testcokey3 <- "19722620"
testcoiid3 <- dbcrosswalk %>% filter(cokey == testcokey3) %>% dplyr::select(coiid) %>% as.character()
testfuzzies3 <- outdata %>%
  filter(cokey == testcokey3) %>%
  left_join(t.nasisjoin)

testindata3 <- indata %>%
  filter(cokey == testcokey3)

errcheck %>% filter(cokey == testcokey3) %>% dplyr::select(cokey, xeric)

####### fuzzies, now reordered to match nasis export
## overall
# 0.192688 to 0.212
testfuzzies3$fuzzvf

## clim subrule
# 0.04283205 to 0.053
testfuzzies3$fuzzclimate

## Non-Xeric Air Temperature, Valley Fever
# 0 to 0
testfuzzies3$fuzzairtempNX

# heating factor / Slope Heating Factor Subrule for Valley Fever
### 0.081 to 0.082
testfuzzies3$fuzzheatingfactor

# aspect / Slope Aspect Factor valley Fever
### 0.1 to 0.1
testfuzzies3$fuzzaspect

# albedo / albedo
# 0.856 to 0.856
### does require input data be inverted, no idea why, but the numbers match exactly so hooray
### look into this when subbing in dsm data
testfuzzies3$fuzzalbedo

# slope heat load / Slope Gradient Valley Fever Heat Loading
# 0.95 to 0.959
testfuzzies3$fuzzslopeheatload

# airtemp / Xeric Air Temperature, Valley Fever
### 0.6 to 0.523
testfuzzies3$fuzzairtempX

# surftempNX / Non-Xeric Air Temperature Limiter, Valley Fever ### nb that the nasis names for X/NX vary
### 0.5783784 to 0.712
testfuzzies3$fuzzsurftempNX

# surftemp X / Xeric Surface Temperature Limiter, Valley Fever
### .7 to .9
testfuzzies3$fuzzsurftempX

# precipNX / Non-Xeric Precipitation, Valley Fever
### .911 to .904
testfuzzies3$fuzzprecipNX

# precipX / Xeric Precipitation, Valley Fever
### 0.91 / 1
testfuzzies3$fuzzprecip

# om / Organic Carbon, kg/m2 to 30cm
### 0.9 to 0.864
testfuzzies3$fuzzom

# water gathering surface  / Water and Spore Gathering Surface
### 0.8 to 0.894
testfuzzies3$fuzzwatergatheringsurface


# chem overall / Chemical Subrule for Valley Fever
### 1 to 0.894. this is a max, both coming from gypsum
testfuzzies3$fuzzchem

# sar / SAR, WTD_AVG to 30cm: 
### 0 to 0, still untested
testfuzzies3$fuzzsar

# ec / Valley Fever EC WTD_AVG 0 to 30 cm, 8-16mmhos: 
### 0 to 0
testfuzzies3$fuzzec

# ph / Soil Reaction WTD_AVG 0-30cm
# 0.0179 to 0, after the sig scale change
testfuzzies3$fuzzph

# gypsum content / Gypsum 0 to 15% within 30cm:
### 1 to 0.894
## still untested
testfuzzies3$fuzzgypsumcontent

# wrd / WRD 0 to 30cm, 5 to 11 percent
### 0.8500471 to 0.946
testfuzzies3$fuzzwrd

# saturation / Valley Fever Near Surface Saturation
### 1 to 1
testfuzzies3$fuzzsaturation

#### RUN ON RASTER

#### VALIDATE ON RASTER (WHERE?)



#return(outdata)
#}

### error summary, 


##### TUNING SIG SCALE ####
testsigscale.in <- indata %>% left_join(t.nasisjoin)
testsigscale.out <- indata %>% dplyr::select(cokey, coiid, comp_name, mukey, muiid, xeric)

testsigscale.in$aspectfactor = if_else( ## this is translated from cvir "VALLEY FEVER ASPECT FACTOR", property 35987
  is.na(testsigscale.in$aspect), 0,
  if_else(
    testsigscale.in$aspect >= 0 & testsigscale.in$aspect < 0, 0,
    if_else(
      testsigscale.in$aspect >= 80 & testsigscale.in$aspect < 280, -((testsigscale.in$aspect-180)**2)/9000+1,
      0 # if all false
    )
  )
)

## sapply to test for lowest r2
## need to get validation data, entered in from nasis graphs. by hand.
testsigscale.valdata <- read.csv("../../nasis equation xy/vf_allxy.csv")

fn.tuneVFscale <- function(eid, incol, refcol, v.scales = 1:100/10){
 # incol <- testsigscale.valdata$sar.x
 # refcol <- testsigscale.valdata$sar.y
 # 
 # eid <- 42999
 # v.scales = 1:20/10

 evalrow <- evals %>% filter(evaliid == eid) %>% dplyr::select(evalname, evaluationtype)
 print(evalrow)
 if(evalrow$evaluationtype != "Sigmoid") stop("This is not a sigmoid evaluation")

 m.vals <- sapply(v.scales, 
        evalbyeid, eid = eid, d = incol)
 m.r2 <- (m.vals - refcol)^2
 m.r2mean <- colMeans(m.r2, na.rm = T)
 names(m.r2mean) <- v.scales
 minr2 <- m.r2mean[m.r2mean == min(m.r2mean)]
 bestsigscale <- names(minr2)
 out <- list(bestsigscale, minr2)
 names(out) <- c("BestParameter", "LowestR2")
 
 if(m.r2mean[1] == minr2) warning("R2 optimizes at lowest parameter, expand range and repeat")
 if(m.r2mean[length(m.r2mean)] == minr2) warning("R2 optimizes at highest parameter, expand range and repeat")

 return(out)
}

fn.tuneVFscale(eid = 42999,
               incol = testsigscale.valdata$sar.x,
               refcol = testsigscale.valdata$sar.y)
# SAR: 5

# EC: not sigmoid
# gypsum: not sigmoid

fn.tuneVFscale(eid = 42985,
               incol = testsigscale.valdata$ph.x,
               refcol = testsigscale.valdata$ph.y,
               v.scales = 1:1000/1000)
# ph: 0.125 

# precips: not sigmoid
# albedo: not sigmoid
# slope heat load: not sigmoid
# aspect: not sigmoid
# surf temp: not sigmoid
# air temp: not sigmoid
# water gathering surface: not sigmoid

fn.tuneVFscale(eid = 42987,
               incol = testsigscale.valdata$wrd.x,
               refcol = testsigscale.valdata$wrd.y,
               v.scales = 1:1000/100)
# wrd: 1.96 (round to 2)
# om: not sigmoid
# fuzz saturation: not sigmoid
# fuzz flood: not sigmoid
