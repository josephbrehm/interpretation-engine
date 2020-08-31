### 0 get all dependencies and prepare workspace
require(data.tree)
require(tidyverse)

# pullSDA is a helper function that uses SoilDB to download ~10 tables into a single list object, from specified area symbols
source("pullSDA.r") 
asym <- c("UT686", "UT685")

# these are the two data.tree objects containing all of the rulesets. They are defined in another file.
load("hsg-datatree.rdata")
load("svi-datatree.rdata")

print(tr.hsg)
print(tr.svi)

# the statement to be evaluated at each decision node is stored in an attribute named "logical"
print(tr.hsg, "logical")

# tree_eval is the function which evaluates the above trees
source("tree_eval.r")

### 1.1 get data for testing ####
l.t <- pullSDA(asym)

## hsg requires: 
## depth to restrictive layer (rl)
## depth to water table (wt)
## ksat at the least restrictive layer that is above the water table (ksat)

## svi requires:
## hsg
## slope
## erodibility factor (kw)

t.rl <- 
  l.t$corestrictions %>%
  select(cokey,
         resdept_r) %>%
  filter(!is.na(cokey))

t.wt <- 
  l.t$cosoilmoist %>%
  inner_join(l.t$comonth %>% select(cokey, comonthkey),
             by = "comonthkey") %>%
  filter(str_trim(tolower(soimoiststat)) == "wet") %>%
  filter(!is.na(cokey)) %>%
  group_by(cokey) %>%
  summarize(wt_min = min(soimoistdept_r, na.rm = T),
            .groups = "drop")
  
t.ksat <-
  l.t$chorizon %>%
  inner_join(t.wt,
             by = "cokey") %>%
  filter(hzdept_r <= wt_min) %>%
  filter(!is.na(cokey)) %>%
  group_by(cokey) %>%
  summarize(ksat_min_abovewt = min(ksat_r),
            .groups = "drop")

t.slope <-
  l.t$component %>%
  filter(!is.na(cokey)) %>%
  select(cokey,
         slope_r)

t.kw <-
  l.t$chorizon %>%
  filter(!is.na(cokey)) %>%
  arrange(cokey,
          hzdept_r) %>%
  group_by(cokey) %>%
  summarize(kwfact = first(kwfact))
  
t.join <- 
  full_join(t.ksat,
            t.kw,
            by = "cokey") %>%
  full_join(t.rl,
            by = "cokey") %>%
  full_join(t.slope,
            by = "cokey") %>%
  full_join(t.wt,
            by = "cokey") %>%
  rename(rl = resdept_r,
         ksat = ksat_min_abovewt,
         kw = kwfact,
         slope = slope_r,
         wt = wt_min)
  
head(t.join)

### 1.2 run the functions ####
t.join$hsg <-
  sapply(1:nrow(t.join),
         function(r){
           tree_eval(
             tree = tr.hsg,
             data = t.join[r,]
           )
         })

head(t.join)

### dual HSG's must be shortened for the svi calculation, eg A/D needs to become A
t.join$shorthsg <- 
  sapply(t.join$hsg,
       function(h){
         strsplit(h, 
                  "/")[[1]][1]
       })
  
t.join$svi <-
  sapply(1:nrow(t.join),
         function(r){
           tree_eval(
             tree = tr.svi,
             data = t.join[r,]
           )
         })

head(t.join)