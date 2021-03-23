#### 1 set up ####
###
setwd("E:/NMSU/interp-engine-personal/TEST FULL WORKFLOW")
outfolder <- "Output"

### packages
# require(sf)
# require(tidyverse)
# require(plyr)
# require(XML)
# require(digest)
# require(data.tree)
# require(dplyr)
# require(rasterVis)
# require(raster)








### run params ###
state <- "UT" # has to be one at a time, due to the way the property data was generated. Even if data below state level is needed, specify the state
asym <- "UT" # can be the same val as state, use that to get the whole state data
county <- "GRAND"

# if you already have a list of NASIS coiids for which you want data, enter it here as a vector and skip to ###STEP###
v.coiid <- NULL 

outname <- "UTGrand250" # prefixed to all outputs 
res <- 250 # resolution for all the rasters


# aea
crs <- crs("+init=epsg:5070")

# set study area polygon (save a sf file to "sf.studyarea")
sf.studyarea <-
  sf::st_read(dsn = "E:/NMSU/GIS/Utah_County_Boundaries-shp", layer = "Counties") %>%
  st_make_valid() %>% st_transform(crs)

if(!is.null(county))  sf.studyarea <- sf.studyarea %>% subset(NAME == county)

#plot(sf.studyarea[1]) # just to make sure there is actual spatial data there

# set path to a local gssurgo (needed for the rasterization step)
path.gssurgo <- paste0("../../../Data/gSSURGO/gSSURGO_", state, ".gdb")


### local funs
source("E:/NMSU/interp-engine-personal/Functions/dwb_calc2.r")
source('../../Functions/local-functions.R')
source("../../Functions/pull_SDA2_compandup.r")

### local data
dbcrosswalk <-
  read.csv(paste0("../../dbcrosswalk/dbcrosswalk_inner_", state, ".csv"))

# this data cache is essential, it contains all the evaluations data
load('../cached-NASIS-data.Rda')

## this all moved to "DWB 1 Get Property Data" in the full workflow test area ####
# #### 2 get property data (EXTREMELY SLOW!) ####
# #### properties are the nasis-specific data needed by the nasis interps
# ### this is done by querying local nasis for component level data. The data must be downloaded in NASIS
# ### first we need a list of component ID's for the specified area
# ### method here accesses all the coiids for a state, downloading them from SDA
# 
# ### need a list of component keys to access. this is created by getting all the components from the specified state
# sda <- pull_SDA_compup(asym = state, fun = "like")
# v.cokey <- sda$component$cokey %>% unique() %>% as.integer()
# 
# 
# ### take the list of cokeys, turn it into a list of coiids, because the data we need is in NASIS
# v.coiid <- dbcrosswalk[dbcrosswalk$cokey %in% v.cokey, "coiid"] %>% as.character() %>% as.vector()
# 
# ### loading the nasis data is slow, and *must* save intermediates along the way. This code does this by saving chunks of 100 components at a time
# v.splitcoiid <- split(v.coiid, ceiling(seq_along(v.coiid)/100))
# 
# ### load the interp tree from the nasis data cache
# dt <- initRuleset('ENG - Dwellings With Basements')
# ps <- getPropertySet(dt)
# 
# ### 2.1 write property data to disk###
# ### iterate through the split list, pulling in the properties specified in 'ps' and saving them. THIS IS EASILY THE SLOWEST STEP
# for(i in 1:length(v.splitcoiid)){
#   
#   name <- paste0("Table Properties/DWBpropertyChunk", state, i, ".rdata")
#   print(name)
#   
#   if(file.exists(name))
#   {
#     ## all this code does is write the files; no action taken if the file exists   
#   } else {
#     vv <- v.splitcoiid[[i]]
#     l.props <- list()
#     
#     for(j in 1:length(vv)){
#       l.props[[j]] <- lookupProperties(
#         coiid = vv[j],
#         propIDs = unique(ps$propiid)
#       )
#     }
#     
#     save(
#       l.props,
#       file = outname
#     )
#   }
# }
# 
# ### 2.2 read property chunks back into R ###
# t.nasisdata <- data.frame(propiid = "",
#                           coiid = "",
#                           comp_name = "",
#                           comp_pct = "",
#                           rv = "")
# v.paths <- list.files(path = "Table Properties", pattern = paste0("DWBpropertyChunk", state), full.names = T)
# 
# for(i in 1:length(v.paths)){
#   load(v.paths[i])
#   #print(i)
#   
#   t <- do.call(rbind, l.props)
#   
#   t.nasisdata <- rbind(t.nasisdata, t)
# }
# 
# t.nasisdata$coiid <- as.character(t.nasisdata$coiid)
# dbcrosswalk$coiid <- as.character(dbcrosswalk$coiid)
# 
# t.nasisjoin <- 
#   left_join(
#     t.nasisdata,
#     ps[,c("propiid", "propname")], 
#     by = "propiid"
#   ) %>%
#   distinct() %>%
#   pivot_wider(
#     id_cols = c(coiid, comp_name),
#     names_from = propname,
#     values_from = rv,
#     values_fn = first) %>%
#   left_join(
#     dbcrosswalk
#   ) 
# 
# head(t.nasisjoin) 
# # this should have ~32 variables, all with nasis names like `DEPTH TO CEMENTED PAN THIN, BELOW O HORIZON`.
# # there will be many NA's!
# 
# ## rename the nasis data to more concise names, ensure all data types are correct, and process the non-numeric rasters into logicals
# t.nasisrename <- 
#   t.nasisjoin %>% dplyr::rename(
#     depbrockhard = `DEPTH TO BEDROCK HARD, BELOW O HORIZON`,
#     fragvol_wmn = `FRAGMENTS > 75MM WEIGHTED AVE. IN DEPTH 0-100CM`,
#     permdepth = `DEPTH TO PERMAFROST`,
#     floodfreq = `FLOODING FREQUENCY (Maximum Frequency)`,
#     depbrocksoft = `DEPTH TO BEDROCK SOFT, BELOW O HORIZON`,
#     lep = `LEP WTD_AVG 25-150cm OR ABOVE RESTRICTION, BELOW O HORIZON`,
#     slope_r = `SLOPE`,
#     depcementthick = `DEPTH TO CEMENTED PAN THICK, BELOW O HORIZON`,
#     restricthardness = `RESTRICTIVE FEATURE HARDNESS`,
#     depcementthin = `DEPTH TO CEMENTED PAN THIN, BELOW O HORIZON`,
#     totalsub_r = `SUBSIDENCE TOTAL`,
#     wt = `HIGH WATER TABLE DEPTH MINIMUM`,
#     gypsum = `SUBSIDENCE DUE TO GYPSUM, REV`,
#     bottomtexture = `UNIFIED BOTTOM LAYER`,
#     texmod = `USDA TEXTURE MODIFIER`,
#     texinlieu = `USDA TEXTURE (IN-LIEU-OF)`,
#     ponddur = `PONDING DURATION`,
#     pondfreq = `PONDING FREQUENCY`,
#     unstablefill = `COMPONENT LOCAL PHASE UNSTABLE FILL`,
#     impaction = `COMPONENT LOCAL PHASE IMPACTED`,
#     drainageclass = `DRAINAGE CLASS IS NOT SUBAQUEOUS`
#   ) %>% 
#   mutate(
#     texinlieu = as.factor(texinlieu),
#     texmod = as.factor(texmod),
#     permdepth = as.numeric(permdepth),
#     pondfreq = as.factor(pondfreq),
#     ponddur = as.factor(ponddur),
#     slope_r = as.numeric(slope_r),
#     totalsub_r = as.numeric(totalsub_r),
#     floodfreq = as.factor(floodfreq),
#     wt = as.numeric(wt),
#     lep = as.numeric(wt),
#     bottomtexture = as.factor(bottomtexture),
#     depbrockhard = as.numeric(depbrockhard),
#     depbrocksoft = as.numeric(depbrocksoft),
#     fragvol_wmn = as.numeric(fragvol_wmn),
#     restricthardness = as.factor(restricthardness),
#     depcementthick = as.numeric(depcementthick),
#     depcementthin = as.numeric(depcementthin),
#     unstablefill = as.factor(unstablefill),
#     gypsum = as.numeric(gypsum),
#     impaction = as.factor(impaction),
#     drainageclass = as.factor(drainageclass),
#     mukey = as.character(mukey),
#     cokey = as.character(cokey),
#     lkey = as.character(lkey),
#     muiid = as.character(muiid),
#     coiid = as.character(coiid),
#     liid = as.character(liid),
#     dmuiid = as.character(dmuiid),
#     comppct = as.integer(comppct)
#   ) %>% # limit to only majority components, so it can be one to a map unit
#   arrange(
#     mukey,
#     -comppct
#   ) %>%
#   group_by(
#     mukey
#   ) %>%
#   summarise_all(
#     first
#   ) %>% # do the conversion to logicals here
#   mutate(
#     pftex = grepl(pattern = "cpf", x = texinlieu) | grepl(pattern = "pf", x = texmod),
#     ponding = 
#       (tolower(str_trim(ponddur)) %in% c("very brief", "brief", "long", "very long") & !is.na(ponddur)) |
#       (tolower(str_trim(pondfreq)) != "none" & !is.na(pondfreq)),
#     flooding = tolower(str_trim(floodfreq)) %in% c("very rare", "rare", "occasional", "frequent", "very frequent"),
#     organicsoil = grepl("(pt)|(ol)|(oh)", bottomtexture),
#     noncemented = tolower(str_trim(restricthardness)) == "noncemented",
#     unstablefill = unstablefill == "1",
#     impaction = impaction == "1",
#     drainageclass = drainageclass == "1"
#   )
# 
# head(t.nasisrename) # should have ~5 more columns than t.nasisjoin, and they should all have more concise names
# t.nasisrename <- dwb_calc2(indata = t.nasisrename) 
# summary(as.factor(t.nasisrename$dwb)) # this should work! The code will run on any data frame with the right data and colnames

## and this moved to dwb 2 in the same place ####
#### 3 get map unit polygons from a local gssurgo & attach data ####
sf.mupolygon <- sf::st_read(dsn = path.gssurgo, layer = "MUPOLYGON") %>%
  dplyr::rename(mukey = MUKEY) %>% st_transform(crs) %>% st_make_valid()

# limit to a test area ##
sf.mupolygon.subset <-
  st_intersection(sf.mupolygon, sf.studyarea)

# plot(sf.mupolygon.subset[1]) # again, make sure the spatial data is still here

# attach all the nasis data to the study area polygon(s)
sf.nasis <-
  left_join(sf.mupolygon.subset,
            t.nasisrename,
            by = "mukey")

head(sf.nasis) # again, v similar to the other head() calls. 
# But it should have a shape column, all the columns specified below in vec.colnames.dwb, and some unneeded data

## aaaand this one to dwb 3 ####
#### 4 polygon to raster (SLOW!) ####
# create a template raster with the extent, crs, and resolution needed
r.template <-
  raster(
    extent(sf.studyarea),
    resolution = res,
    crs = crs
  )

# name those layers! These must all be present as columns in t.nasisrename (and passed on to sf.nasis)
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
    print(name)
    if(file.exists(name)){
      r <- raster(name)
    } else {
      p <- sf.nasis[l]
      r <- rasterize(
        x = p,
        field = l,
        y = r.template,
        filename = name,
        driver = "GTiff"
      ) 
    }
    return(r)
  }
)

## send those rasters into a brick
brk <- brick(l.r) %>% projectRaster(crs = crs) %>% mask(sf.studyarea)

names(brk) <- vec.layernames.dwb

### actual calculations, using the processed raster brick #### 
### AT THIS POINT, DSM PRODUCTS CAN BE SUBBED IN FOR ANY LAYERS HERE! The names & expected data types absolutely must match though

# pass the brick into a data frame and run the calc function (can this switch to raster::calc()?)
df <- as.data.frame(brk)
df <- dwb_calc2(df)
summary(df$dwb)


# now send it back to the brick
brk$dwb <- brk$wt %>% setValues(factor(df$dwb), levels = rev(c("Not limited", "Somewhat limited", "Very limited"))) %>% mask(sf.studyarea)
brk$maxfuzz <- brk$wt %>% setValues(df$maxfuzz) %>% mask(sf.studyarea)

levelplot(brk$dwb, main = "Dwellings with Basements Interpretation")
plot(brk$maxfuzz)

### save data ####

writeRaster(brk$dwb, file = paste0(outfolder, "/", outname, "_dwb.tif"), overwrite = T)
writeRaster(brk$maxfuzz, file = paste0(outfolder, "/", outname, "_dwb_maxfuzzyrating.tif"), overwrite = T)


st_write(sf.nasis %>% dplyr::select(dwb), 
         dsn = paste0(outfolder, "/", outname, "_dwb_fromnasis.shp"), 
         #name = paste0(outname, "_nasisdata.shp"),
         driver = "ESRI Shapefile",
         append = F)

### add in a dsm slope raster

brk2 <- brk
r.slope <- raster("E:/NMSU/Data/Slope/slope_pct_utGrand250.tif") %>% projectRaster(brk)
brk2$slope_r <- r.slope

df2 <- as.data.frame(brk2)
df2 <- dwb_calc2(df2)
summary(df2$dwb)

# now send it back to the brick
brk2$dwb <- brk2$wt %>% setValues(factor(df2$dwb), levels = rev(c("Not limited", "Somewhat limited", "Very limited"))) %>% mask(sf.studyarea)
brk2$maxfuzz <- brk2$wt %>% setValues(df2$maxfuzz) %>% mask(sf.studyarea)

levelplot(brk2$dwb, main = "Dwellings with Basements Interpretation")
plot(brk2$maxfuzz)

writeRaster(brk2$dwb, file = paste0(outfolder, "/", outname, "_dwb_withDSMdata.tif"), overwrite = T)
