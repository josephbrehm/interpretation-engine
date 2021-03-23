require(tidyverse)
require(plyr)
require(XML)
require(digest)
require(data.tree)
require(dplyr)

setwd("E:/NMSU/interp-engine-personal/Engine Guts/DWB")

dbcrosswalk <-
  read.csv("..\\..\\dbcrosswalk\\dbcrosswalk_inner_AK-AZ-CT-NE-UT-NM.csv")

source('../../Functions/local-functions.R')
load('../cached-NASIS-data.Rda')

dt <- initRuleset('ENG - Dwellings With Basements')
ps <- getPropertySet(dt)

### 1.2 load data chunks back into R, reshape it ####
t.nasisdata <- data.frame(propiid = "",
                          coiid = "",
                          comp_name = "",
                          comp_pct = "",
                          rv = "")
v.paths <- list.files(pattern = "DWBpropertyChunk")

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
#asym <- c('AK651', 'CT600', 'NE043', 'UT616', 'NM632')

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

#save(indata.dwb.nasis, file = "indata.dwb.nasis.big1.rda")
#save(indata.dwb.nasis, file = "indata.dwb.nasis.rda")