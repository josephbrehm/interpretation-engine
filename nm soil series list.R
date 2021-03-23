# get colby's new mexico soil series sheet
# col 1: area symbol
# col 2: map unit id, SDA
# col 3: soil series
# col 4: list of components

require(tidyverse)
require(sf)
setwd("E:/NMSU/NM Soil Series List")
source("../interp-engine-personal/Functions/pull_SDA2_compandup.R")


## load tables from SDA. It's not real data though -- just use to find table connections


indata <-  
  pull_SDA_compup(
    asym = "NM",
    fun = "like"
  )
  
indata$component$compname # these are series names, mostly. They repeat (actual hooray)
indata$component$compkind # notes which are series

proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

df.join <-
  indata$component %>%
  dplyr::select(
    compname,
    compkind,
    comppct_r,
    cokey,
    mukey
  ) %>%
  mutate_if(
    is.character, str_trim
  ) %>%
  mutate_if(
    is.character, proper
  ) %>%
  filter(
    compkind == "Series"
  ) %>%
  left_join(
    indata$mapunit,
    by = "mukey"
  ) %>%
  left_join(
    indata$legend,
    by = "lkey"
  ) %>%
  dplyr::select(
    areasymbol,
    lkey,
    muname,
    mukey,
    compname,
    cokey,
    comppct_r
  )

head(df.join)

fn.dbcollapse = function(s, col, df){
  df.subset = subset(df, compname == s)
  v.out = paste(unique(df.subset[,col]), collapse = "; ")
  return(v.out)
}

fn.dbcollapse(
  s = "Gila",
  col = "cokey",
  df = df.join
)

df.collapse <- data.frame(
  series = unique(df.join$compname),
  all.areasymbol = sapply(unique(df.join$compname),
                          FUN = fn.dbcollapse,
                          col = "areasymbol",
                          df = df.join),
  all.lkey = sapply(unique(df.join$compname),
                    FUN = fn.dbcollapse,
                    col = "lkey",
                    df = df.join),
  all.muname = sapply(unique(df.join$compname),
                      FUN = fn.dbcollapse,
                      col = "muname",
                      df = df.join),
  all.mukey = sapply(unique(df.join$compname),
                     FUN = fn.dbcollapse,
                     col = "mukey",
                     df = df.join),
  all.cokey = sapply(unique(df.join$compname),
                     FUN = fn.dbcollapse,
                     col = "cokey",
                     df = df.join),
  all.comppct = sapply(unique(df.join$compname),
                       FUN = fn.dbcollapse,
                       col = "comppct_r",
                       df = df.join),  
  row.names = NULL
  )

head(df.collapse)

write.csv(df.collapse, file = "NM Soil Series by Series.csv", row.names = F)
write.csv(df.join, file = "NM Soil Series by Component.csv", row.names = F)
