require(tidyverse)
require(plyr)
require(XML)
require(digest)
require(data.tree)
require(dplyr)

setwd("E:/NMSU/interp-engine-personal/Engine Guts/DWB")

dbcrosswalk <-
  read.csv("..\\..\\dbcrosswalk\\dbcrosswalk_inner_AK-AZ-CT-NE-UT-NM.csv")

state <- "CT"


### old. need all cokeys in the state
# load("indata.dwb.rda")
# indata <- indata.dwb ### just used to get cokey list
source("../../Functions/pull_SDA2_compandup.r")

# ak <- pull_SDA_compup(asym = "AK", fun = "like")
# az <- pull_SDA_compup(asym = "AZ", fun = "like")
# ct <- pull_SDA_compup(asym = "CT", fun = "like")
# ne <- pull_SDA_compup(asym = "NE", fun = "like")
# nm <- pull_SDA_compup(asym = "NM", fun = "like")
# ut <- pull_SDA_compup(asym = "UT", fun = "like")
sda <- pull_SDA_compup(asym = state, fun = "like")


v.cokey <- sda$component$cokey %>% unique() %>% as.integer()
#   #ak$component$cokey
# # az$component$cokey,
#  ct$component$cokey
# # ne$component$cokey
# #  nm$component$cokey
# # ut$component$cokey
# ) %>% unique() %>% as.integer()

source('../../Functions/local-functions.R')
load('../cached-NASIS-data.Rda')

dt <- initRuleset('ENG - Dwellings With Basements')
ps <- getPropertySet(dt)

# ## 1.1 get and write chunk properties -- commented out for safety, long runtime####
# 
# v.coiid <-
#   inner_join(v.cokey,
#              dbcrosswalk,
#              by = c("cokey" = "key.sda")) %>%
#   select(key.nasis) %>%
#   mutate(key.nasis = as.character(key.nasis))
# v.coiid <- as.vector(v.coiid$key.nasis)

v.coiid <- dbcrosswalk[
  dbcrosswalk$cokey %in% v.cokey,
  "coiid"
] %>% as.character() %>% as.vector()

v.splitcoiid <- split(v.coiid, ceiling(seq_along(v.coiid)/100))


for(i in 1:length(v.splitcoiid)){

  outname <- paste0("DWBpropertyChunk", state, i, ".rdata")
  print(outname)

  if(file.exists(outname))
  {

  } else {
    vv <- v.splitcoiid[[i]]

    l.props <- list()

    for(j in 1:length(vv)){
      #for(j in 1:1){

      l.props[[j]] <- lookupProperties(
        coiid = vv[j],
        propIDs = unique(ps$propiid)

      )
    }

    save(
      l.props,
      file = outname
    )
  }
}

# l.props
# 
### 1.2 load data chunks back into R, reshape it ####
t.nasisdata <- data.frame(propiid = "",
                          coiid = "",
                          comp_name = "",
                          comp_pct = "",
                          rv = "")
v.paths <- list.files(pattern = "DWBpropertyChunk2-")

for(i in 1:length(v.paths)){
  load(v.paths[i])
  
  t <- do.call(rbind, l.props)
  
  t.nasisdata <- rbind(t.nasisdata, t)
}

t.nasisjoin <- 
  left_join(
    t.nasisdata,
    ps[,c("propiid", "propname")], 
    by = "propiid"
  ) %>%
  select(
    coiid, comp_name, rv, propname
  ) %>% 
  distinct() %>%
  pivot_wider(
    id_cols = c(coiid, comp_name),
    names_from = propname,
    values_from = rv
  )  

head(t.nasisjoin)

### attach sda data (ie, the correct answer for dwb)

source("..\\..\\Functions\\pull_SDA2.r")
asym <- c('AK651', 'CT600', 'NE043', 'UT616', 'NM632')

l.sda <- list()
l.sda$ak <-   
  soilDB::SDA_query(paste0(
    "WITH c AS (SELECT cokey 
                FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                INNER JOIN component ON component.mukey = mapunit.mukey
                WHERE areasymbol LIKE '%AK%')
     SELECT *
     FROM c
     INNER JOIN cointerp ON c.cokey = cointerp.cokey
     WHERE mrulename = 'ENG - Dwellings With Basements'")) %>%
  dplyr::select(-1)

l.sda$ct <-   
  soilDB::SDA_query(paste0(
    "WITH c AS (SELECT cokey 
                FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                INNER JOIN component ON component.mukey = mapunit.mukey
                WHERE areasymbol LIKE '%CT%')
     SELECT *
     FROM c
     INNER JOIN cointerp ON c.cokey = cointerp.cokey
     WHERE mrulename = 'ENG - Dwellings With Basements'")) %>%
  dplyr::select(-1)

l.sda$ne <-   
  soilDB::SDA_query(paste0(
    "WITH c AS (SELECT cokey 
                FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                INNER JOIN component ON component.mukey = mapunit.mukey
                WHERE areasymbol LIKE '%NE%')
     SELECT *
     FROM c
     INNER JOIN cointerp ON c.cokey = cointerp.cokey
     WHERE mrulename = 'ENG - Dwellings With Basements'")) %>%
  dplyr::select(-1)

l.sda$az <-   
  soilDB::SDA_query(paste0(
    "WITH c AS (SELECT cokey 
                FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                INNER JOIN component ON component.mukey = mapunit.mukey
                WHERE areasymbol LIKE '%AZ%')
     SELECT *
     FROM c
     INNER JOIN cointerp ON c.cokey = cointerp.cokey
     WHERE mrulename = 'ENG - Dwellings With Basements'")) %>%
  dplyr::select(-1)

l.sda$nm <-   
  soilDB::SDA_query(paste0(
    "WITH c AS (SELECT cokey 
                FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                INNER JOIN component ON component.mukey = mapunit.mukey
                WHERE areasymbol LIKE '%NM%')
     SELECT *
     FROM c
     INNER JOIN cointerp ON c.cokey = cointerp.cokey
     WHERE mrulename = 'ENG - Dwellings With Basements'")) %>%
  dplyr::select(-1)

l.sda$ut <-   
  soilDB::SDA_query(paste0(
    "WITH c AS (SELECT cokey 
                FROM legend
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                INNER JOIN component ON component.mukey = mapunit.mukey
                WHERE areasymbol LIKE '%UT%')
     SELECT *
     FROM c
     INNER JOIN cointerp ON c.cokey = cointerp.cokey
     WHERE mrulename = 'ENG - Dwellings With Basements'")) %>%
  dplyr::select(-1)

t.sda <- do.call(rbind, l.sda)


refdata <- 
  t.sda %>%
  filter(
    rulename == "ENG - Dwellings With Basements"
  ) %>% 
  inner_join(
    dbcrosswalk,
    by = c("cokey" = "key.sda")
  ) %>%
  mutate(
    key.nasis = as.character(key.nasis)
  ) %>%
  select(
    cokey,
    interpll,
    interpllc,
    interplr,
    interplrc,
    interphr,
    interphrc,
    interphh,
    interphhc,
    key.join,
    key.nasis
  )

head(refdata)

indata.dwb.nasis <-
  inner_join(
    t.nasisjoin,
    refdata,
    by = c("coiid" = "key.nasis")
  )





head(indata.dwb.nasis)

#save(indata.dwb.nasis, file = "indata.dwb.nasis.big.rda")
save(indata.dwb.nasis, file = "indata.dwb.nasis.rda")