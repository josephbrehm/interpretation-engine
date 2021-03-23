### bricklayer: runs all generic raster processing to get a list of rasters into a single brick

## to do:
### intermediate output options
### auto detect resolution

bricklayer <- function(l.r, 
                        crs,
                        shp, # this should be allowable as null, in which case function will find min extent of all rasters
                        res = NULL, # if res is not specified, will resample everything to the res of the first raster in the list
                        verbose = F,
                        saveIntermediates = T,
                        useIntermediates = T,
                        dirIntermediates = "Bricklayer Processing",
                        prefix = NULL
                       ){
  l.r.load <- sapply(l.r, raster)
  
  if(!is.null(prefix)) prefix <- paste0(prefix, "_") #
  
  dir.create(dirIntermediates, showWarnings = F) # no action if directory exists
  
  
  # Initial clip: trim each raster down to approximate final area
  # bboxes will vary due to crs, origin, resolution, etc variation
  # but the working rasters will be much smaller
  if(verbose) print("Processing step 1, rough clip")
  l.r.clip1 <- 
    sapply(l.r.load, function(r){ 
      
      if(verbose) print(names(r))
      
      outname <- paste0(dirIntermediates, "/", prefix, names(r), "_1roughclip.tif")

      if(useIntermediates & file.exists(outname)) {
        r.out <- raster(outname)
      } else {
        minextent <- spTransform(x = shp, CRSobj = crs(r)) %>% extent()
        r.out <- raster::extend(r, minextent) %>% 
          raster::crop(minextent)
        if(saveIntermediates) {
          writeRaster(r.out, filename = outname)
        }
      }
      
      names(r.out) <- names(r)
      
      return(r.out)
      
    })
  
  rm(l.r.load) # is this helpful within a function?
  
  # should change this to calculating min resolution -- would have to reproject all resolutions
  if(is.null(res)) {
    res <- res(l.r.clip1[[1]])
  }
  
  if(verbose) print("Processing step 2, reproject")
  l.r.project <- 
    sapply(l.r.clip1, function(r){
      if(verbose) print(names(r))
      
      outname <- paste0(dirIntermediates, "/", prefix, names(r), "_2project.tif")
      
      if(useIntermediates & file.exists(outname)) {
        r.out <- raster(outname)
      } else {
        r.out <- projectRaster(from = r,
                               res = res,
                               crs = crs)
        if(saveIntermediates) {
          writeRaster(r.out, filename = outname)
        }
      }
      
      names(r.out) <- names(r)
      
      return(r.out)
        
    })
  
  rm(l.r.clip1)
  
  if(verbose) print("Processing step 3, resample")
  l.r.resample <-
    sapply(l.r.project, function(r){
      if(verbose) print(names(r))
      
      outname <- paste0(dirIntermediates, "/", prefix, names(r), "_3resample.tif")
      
      if(useIntermediates & file.exists(outname)) {
        r.out <- raster(outname)
      } else {
        r.out <- raster::resample(x = r, y = l.r.project[[1]])
        if(saveIntermediates) {
          writeRaster(r.out, filename = outname)
        }
      }
      
      names(r.out) <- names(r)
      
      return(r.out)
      
    })
  
  rm(l.r.project)
  
  if(verbose) print("Processing step 4, final clip")
  l.r.clip2 <-
    sapply(l.r.resample, function(r){
      if(verbose) print(names(r))
      
      
      outname <- paste0(dirIntermediates, "/", prefix, names(r), "_4clip.tif")
      
      if(useIntermediates & file.exists(outname)) {
        r.out <- raster(outname)
      } else {
        r.out <- raster::mask(r, shp)
        if(saveIntermediates) {
          writeRaster(r.out, filename = outname)
        }
      }
      
      names(r.out) <- names(r)
      
      return(r.out)
      
    })
  
  brk <- brick(l.r.clip2)
  
  return(brk)
}
