## get dwb properties as rasters
### make baseline default out of the extracted properties
#### tie this to gnatsgo for spatialness
### get some dsm things to sub in

### do this one state at a time 

state = "UT" # has to be one at a time, due to the way the property data was generated
county = NULL
# aea
crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

outname <- "UT5000" # prefixed to all outputs 
res <- 5000 # resolution for all the rasters

library(raster)

##
require(sf)
require(tidyverse)
require(plyr)
require(XML)
require(digest)
require(data.tree)
require(dplyr)
require(rasterVis)
require(raster)

#source("E:/NMSU/interp-engine-personal/Functions/dwb_calc.r")
source("E:/NMSU/interp-engine-personal/Functions/dwb_calc2.r")
source("../../Functions/cm_refactor.R")

setwd("E:/NMSU/interp-engine-personal/Engine Guts/DWB")

dbcrosswalk <-
  read.csv(paste0("..\\..\\dbcrosswalk\\dbcrosswalk_inner_", state, ".csv"))

source('../../Functions/local-functions.R')
load('../cached-NASIS-data.Rda')


### get property chunks ####
dt <- initRuleset('ENG - Dwellings With Basements')
ps <- getPropertySet(dt)

t.nasisdata <- data.frame(propiid = character(),
                          coiid = character(),
                          comp_name = character(),
                          comp_pct = numeric(),
                          rv = character())
v.paths <- list.files(pattern = paste0("DWBpropertyChunk", state))


for(i in 1:length(v.paths)){
  load(v.paths[i])
  print(v.paths[i])

  #t <- do.call(rbind.fill, l.props)
  #t.nasisdata <- rbind.fill(t.nasisdata, t)

  t <- do.call(rbind, l.props)
  print(head(t))
  
  t.nasisdata <- rbind(t.nasisdata, t)
  
}

t.nasisdata$coiid <- as.character(t.nasisdata$coiid)
dbcrosswalk$coiid <- as.character(dbcrosswalk$coiid)

t.nasisjoin <- 
  left_join(
    t.nasisdata,
    ps[,c("propiid", "propname")], 
    by = "propiid"
  ) %>%
   distinct() %>%
  # dplyr::select(
  #   coiid, comp_name, rv, propname
  # ) %>% 
  # distinct() %>%
  pivot_wider(
    id_cols = c(coiid, comp_name),
    names_from = propname,
    values_from = rv,
    values_fn = first) %>%
  left_join(
    dbcrosswalk
  ) 

t.nasisrename <- 
  t.nasisjoin %>% dplyr::rename(
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
    drainageclass = `DRAINAGE CLASS IS NOT SUBAQUEOUS`
  ) %>% 
  mutate(
    texinlieu = as.factor(texinlieu),
    texmod = as.factor(texmod),
    permdepth = as.numeric(permdepth),
    pondfreq = as.factor(pondfreq),
    ponddur = as.factor(ponddur),
    slope_r = as.numeric(slope_r),
    totalsub_r = as.numeric(totalsub_r),
    floodfreq = as.factor(floodfreq),
    wt = as.numeric(wt),
    lep = as.numeric(wt),
    bottomtexture = as.factor(bottomtexture),
    depbrockhard = as.numeric(depbrockhard),
    depbrocksoft = as.numeric(depbrocksoft),
    fragvol_wmn = as.numeric(fragvol_wmn),
    restricthardness = as.factor(restricthardness),
    depcementthick = as.numeric(depcementthick),
    depcementthin = as.numeric(depcementthin),
    unstablefill = as.factor(unstablefill),
    gypsum = as.numeric(gypsum),
    impaction = as.factor(impaction),
    drainageclass = as.factor(drainageclass),
    mukey = as.character(mukey),
    cokey = as.character(cokey),
    lkey = as.character(lkey),
    muiid = as.character(muiid),
    coiid = as.character(coiid),
    liid = as.character(liid),
    dmuiid = as.character(dmuiid),
    comppct = as.integer(comppct)
  ) %>% # limit to only majority components, so it can be one to a map unit
  arrange(
    mukey,
    -comppct
  ) %>%
  group_by(
    mukey
  ) %>%
  summarise_all(
    first
  ) %>% # do the conversion to logicals here
  mutate(
    pftex = grepl(pattern = "cpf", x = texinlieu) | grepl(pattern = "pf", x = texmod),
    ponding = 
      (tolower(str_trim(ponddur)) %in% c("very brief", "brief", "long", "very long") & !is.na(ponddur)) |
      (tolower(str_trim(pondfreq)) != "none" & !is.na(pondfreq)),
    flooding = tolower(str_trim(floodfreq)) %in% c("very rare", "rare", "occasional", "frequent", "very frequent"),
    organicsoil = grepl("(pt)|(ol)|(oh)", bottomtexture),
    noncemented = tolower(str_trim(restricthardness)) == "noncemented",
    unstablefill = unstablefill == "1",
    impaction = impaction == "1",
    drainageclass = drainageclass == "1"
  )
  
  
head(t.nasisrename)

test <- dwb_calc2(t.nasisrename)
summary(as.factor(test$dwb))

### get polygons & attach data ####

path = paste0("../../../Data/gSSURGO/gSSURGO_", state, ".gdb")

sf.mupolygon <- sf::st_read(dsn = path, layer = "MUPOLYGON") %>%
 dplyr::rename(mukey = MUKEY) %>% st_transform(crs) %>% st_make_valid()

# limit to a test area ##

#### this needs to be better

if(state == "UT"){
  sf.studyarea <-
    sf::st_read(dsn = "E:/NMSU/GIS/Utah_County_Boundaries-shp", layer = "Counties") %>%
    #subset(NAME == county) %>%
    st_make_valid() %>% st_transform(crs)
  
  sf.mupolygon.subset <-
    st_intersection(sf.mupolygon, sf.studyarea)
} else if (state == "CT") {
  
  sf.studyarea <-
    sf::st_read(dsn = "E:/NMSU/GIS/tl_2019_us_state", layer = "tl_2019_us_state") %>%
    subset(NAME == "Connecticut") %>%
    st_make_valid() %>% st_transform(crs)
  
  # sf::st_read(dsn = "E:/NMSU/GIS", layer = "TinyCTTestArea") %>%
  # st_make_valid() %>% st_transform(crs)
    
  sf.mupolygon.subset <-
    st_intersection(sf.mupolygon, sf.studyarea)
  
} else  {
  sf.mupolygon.subset <- sf.mupolygon
}


length(unique(t.nasisrename$mukey)) - length(t.nasisrename$mukey)
# above should be 0, otherwise a mukey is duplicated

sf.nasis <-
  left_join(sf.mupolygon.subset,
        t.nasisrename,
        by = "mukey")

head(sf.nasis)

### run dwb calc over the sf object

test2 <- dwb_calc2(as.data.frame(sf.nasis))
sf.nasis$dwb <- as.factor(test2$dwb)
summary(sf.nasis$dwb)
#plot(sf.nasis["dwb"])

### polygon to raster ####
##require(stars)
## stars is frustrating. switch to raster
### need to add in sth to write a reclass matrix for the categoricals

r.template <-
  raster(
    extent(sf.nasis[1]),
    resolution = res,
    crs = crs
  
)

#### this is the old stars code. del it later####
# lazyrasterize = function(sf, res=1000){
#   
#   xlen = abs(sf::st_bbox(sf)[1] - sf::st_bbox(sf)[3])
#   ylen = abs(sf::st_bbox(sf)[2] - sf::st_bbox(sf)[4])
#   
#   xcount = ceiling(xlen / res)
#   ycount = ceiling(ylen / res)
#   
#   cls = class(sf[[1]]) # factors need to be processed differently. the code as previously written codes the pixel id, not the vals
#   
#   if(cls == "factor") {
#     factorcrosswalk <- 
#       data.frame(
#         level = levels(sf[[1]]),
#         rasterval = 1:length(levels(sf[[1]]))
#       )
#   }
#   
#   out <- stars::st_rasterize(sf,
#                              st_as_stars(
#                                st_bbox(sf),
#                                nx = xcount,
#                                ny = ycount
#                              ))
#   
#   return(out)
# } ####
# vec.layernames.dwb <- 
#   c("texinlieu",
#     "texmod",
#     "permdepth",
#     "pondfreq",
#     "ponddur",
#     "slope_r", 
#     "totalsub_r",
#     "floodfreq",
#     "wt",
#     "lep",
#     "bottomtexture",
#     "depbrockhard",
#     "depbrocksoft",
#     "fragvol_wmn",
#     "restricthardness",
#     "depcementthick",
#     "depcementthin",
#     "unstablefill",
#     "gypsum",
#     "impaction",
#     "drainageclass")

vec.layernames.dwb <- 
  c("permdepth", # permafrost depth
    "slope_r", # mean slope 
    "totalsub_r", # total subsidence
    "wt", # depth to water table
    "lep",
    "depbrockhard", # depth to hard bedrock
    "depbrocksoft", # depth to soft bedrock
    "fragvol_wmn", # weighted mean of rock fragments (theres more to this joe)
    "restricthardness",
    "depcementthick",
    "depcementthin",
    "unstablefill",
    "gypsum",
    "impaction",
    "drainageclass",
    "pftex",
    "ponding",
    "flooding",
    "organicsoil",
    "noncemented")

l.r <- sapply(
  vec.layernames.dwb,
  function(l){
    print(l)
    name <- paste0("Raster Properties/", outname, "_", l, ".tif")
    #c <- class(sf.nasis[l][[1]])
    
    # if(c == "factor" & !all(is.na(sf.nasis[l][[1]]))){ ### if a factor, you also need to save the level crosswalk, unless its an all-na field
    #   namecw <- paste0("Raster Properties/levels_", l, ".rds")
    #   if(file.exists(namecw)){
    #     cw <- readRDS(namecw)
    #   } else{
    #     cw <- 
    #       data.frame(
    #         ID = 1:length(levels(sf.nasis[l][[1]])),
    #         code = levels(sf.nasis[l][[1]])
    #       )
    #     
    #     saveRDS(cw, file = namecw)
    #   }
    # }
    
    if(file.exists(name)){
      r <- raster(name)
    } else {
      
      p <- sf.nasis[l]
      
      
      r <- rasterize(
        p,
        field = l,
        r.template,
        filename = name,
        driver = "GTiff"
      ) 
    }


    # if(c == "factor"){
    #   f <- ratify(r)
    #   
    #   if(!all(is.na(sf.nasis[l][[1]]))){
    #     levels(f) <- cw
    #   }
    #   r <- f
    # }
    
    return(r)
  }
)

plot(l.r$slope_r)
#sapply(l.r, plot)

### old stars code
# sapply(
#   (ncol(t.nasisdata) + 1) : ncol(sf.nasis) - 1,
#     function(l){
#   
#   layername = colnames(sf.nasis[l])[1]
#   print(layername)
#   r = lazyrasterize(sf.nasis[layername], 1000)
#   
#   stars::write_stars(r,
#                      dsn = paste0("Raster Properties/", layername, ".tif"),
#                      filename = layername,
#                      layer = 1,
#                      driver = "GTiff")
#   
# })

## load rasters back in as a brick ####
source("E:/NMSU/Functions/bricklayer.r")

vec.layerpaths <- paste0("Raster Properties/", vec.layernames.dwb, ".tif")
  
## add these to the bricklayer
library(raster)
library(tidyverse)

# need study area
#studyarea <- st_union(sf.mupolygon)
#plot(studyarea)


# brk <- bricklayer(
#   l.r = vec.layerpaths,
#   crs = crs,
#   shp = sf.studyarea,
#   res = res,
#   verbose = T,
#   saveIntermediates = T,
#   useIntermediates = T,
#   dirIntermediates = "Bricklayer Processing",
#   prefix = outname
# )
brk <- brick(l.r) %>% mask(sf.studyarea)

names(brk) <- vec.layernames.dwb

#plot(brk)
# levelplot(brk$texmod)
# plot(brk$slope_r)
#plot(sf.nasis["slope_r"])
# levels(brk$texmod)
# brk$texmod


### the bricklayer drops factor levels in like the first step
## add them back in based on whether or not there's an rds
# 
# vec.rdspaths <- paste0("Raster Properties/levels_", vec.layernames.dwb, ".rds")
# hasrds <- vec.layernames.dwb[file.exists(vec.rdspaths)]
# 
# for(l in hasrds){
#   cw <- readRDS(paste0("Raster Properties/levels_", l, ".rds"))
#   r <- brk[[l]]
#   
#   f <- ratify(r)
#   levels(f) <- cw
#   
#   brk[[l]] <- f
#   
# }

# sapply(
#   hasrds,
#   function(l){
#     cw <- readRDS(paste0("Raster Properties/levels_", l, ".rds"))
#     r <- brk[[l]]
#     
#     f <- ratify(r)
#     levels(f) <- cw
#     
#     r <- f
#   }
# )

#plot(brk$slope_r)
#levelplot(brk$texmod)


### run the dwb calc over a brick ####
writeRaster(brk, file = paste0(outname, "_brick.tif"), overwrite = T)

df <- as.data.frame(brk)

df <- dwb_calc2(df)
summary(as.factor(df$dwb))
df$dwb <- as.factor(df$dwb)
summary(df$dwb)

brk$dwb <- 
  brk$wt %>%
  # setValues(factor(df$dwb,
  #                  levels = rev(c("Not limited", "Slightly limited", "Very limited"))))
  setValues(df$dwb) %>% mask(sf.studyarea)
  

levelplot(brk$dwb, main = "Dwellings with Basements Interpretation")
#plot(sf.nasis["dwb"], border = NA)
writeRaster(brk$dwb, file = paste0(outname, "_dwb.tif"), overwrite = T)

st_write(sf.nasis, dsn = paste0("Validation", outname), driver = "ESRI Shapefile")


### results! validate these now ###

## how? 

### tomorrow notes: check the different data 



### can you do dwb calc on the sf object
test <- dwb_calc(as.data.frame(sf.nasis))
sf.nasis$dwb <- test$dwb

#plot(sf.nasis)
#plot(sf.nasis["slope_r"])
#plot(sf.nasis["dwb"])

testr = rasterize(
  sf.nasis["dwb"],
  field = "dwb",
  r.template
) 
plot(testr)

## these can plot, so yes
## almost all sl though, is that right?
## validate the sf nasis data

### loaded in errcheck from the dwbcalc workspace
intersect(colnames(errcheck), colnames(sf.nasis))


cm_refactor(pred = errcheck$dwb,
            ref = errcheck$refdwb)[2]
cm_refactor(pred = errcheck$dwb,
            ref = errcheck$refdwb)[[3]][1] # 97% as of 12-11 # 96.8 on 12/16

errcheck2 <- 
  inner_join(
    sf.nasis,
    errcheck[,c("coiid", "refdwb")]
  )

cm_refactor(pred = errcheck2$dwb,
            ref = errcheck2$refdwb)[2]
cm_refactor(pred = errcheck2$dwb,
            ref = errcheck2$refdwb)[[3]][1]

### the numbers are off with the raster calc. something isnt right
### what are the differences between sf.nasis and the data that goes into errcheck

# sf.nasis draws from nasis property chunks
# dwb workspace from some cached data
# try pulling in the cached data? unclear what the diff is

load("indata.dwb.nasis.big1.rda")
head(indata.dwb.nasis)

sf.nasis$cokey
indata.dwb.nasis$cokey <- as.character(indata.dwb.nasis$cokey)

test <- left_join(sf.nasis, indata.dwb.nasis)

