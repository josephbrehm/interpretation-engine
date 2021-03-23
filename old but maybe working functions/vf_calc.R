require(tidyverse)
require(soilDB)
require(XML)
require(digest)

vf_calc <- function(indata, 
                    doxeric = "auto" # force xeric or nonxeric function, or let it choose for each data row/pixel depending on an input raster (accepts "auto" / "xeric" / "nonxeric")
){ 
  if(!(doxeric %in% c("auto", F, T))){
    print("Unknown input for xeric parameter, which determines whether to process data as xeric or nonxeric conditions")
    print("Accepted input is 'auto', 'T' (process all as xeric), or 'F' (process all as nonxeric).")
    print("Defaulting to auto. This requires data specifying conditions by data point, and will error if not provided")
    doxeric <- "auto"
  }
  
  # if a raster stack or brick is provided as input, the output will be a raster brick
  rasterinput <- class(indata)[1] %in% c("RasterStack", "RasterBrick")
  
  if(rasterinput) {
    require(raster)
    r.template <- indata[[1]] # save the first raster in the obj as a template to create an out raster later
    indata <- as.data.frame(indata)
  }
  
  outdata <- indata %>% dplyr::select(
   # cokey, coiid, comp_name, mukey, muiid, 
    xeric)
  
  ## 1 - chemical subrule
  outdata$fuzzsar <- evalbyeid(42999, indata$sar, sig.scale = 5) %>% replace_na(0)
  outdata$fuzzec <- evalbyeid(43000, indata$ec) %>% replace_na(0)
  outdata$fuzzgypsumcontent <- evalbyeid(42991, indata$gypsumcontent) ^ (0.5) %>% replace_na(0)
  outdata$fuzzph <- evalbyeid(42985, indata$ph, sig.scale = 0.125) %>% replace_na(0)
  
  outdata$fuzzchem <- 
    do.call(pmax, c(outdata[,c('fuzzsar', 'fuzzec', 'fuzzgypsumcontent', 'fuzzph')], na.rm = T))
  
  ## 2 - climatic subrule
  if(!("xeric"  %in% colnames(indata))) indata$xeric <- NA
  if(doxeric != "auto"){
    if(doxeric) {indata$xeric <- 1}
    if(!doxeric) {indata$xeric <- 0}
  }
  
  outdata$fuzzprecipX <- evalbyeid(42997, indata$map)
  outdata$fuzzprecipNX <- evalbyeid(42998, indata$map)
  
  outdata$fuzzalbedo <- evalbyeid(43047, 1 - indata$albedo)
  
  indata$aspectfactor = if_else( ## this is translated from cvir "VALLEY FEVER ASPECT FACTOR", property 35987
    is.na(indata$aspect), 0,
    if_else(
      indata$aspect >= 0 & indata$aspect < 0, 0,
      if_else(
        indata$aspect >= 80 & indata$aspect < 280, -((indata$aspect-180)**2)/9000+1,
        0 # if all false
      )
    )
  )
  
  outdata$fuzzslopeheatload <- evalbyeid(43048, indata$slope)
  outdata$fuzzaspect <- evalbyeid(43049, indata$aspectfactor)
  outdata$fuzzheatingfactor <- 
    outdata$fuzzalbedo *
    outdata$fuzzslopeheatload *
    outdata$fuzzaspect
  
  
  outdata$airtempchopperX <- indata$airtemp / 16
  outdata$airtempchopperNX <- indata$airtemp / 18.5
  
  outdata$fuzzsurftempX <- evalbyeid(43050, outdata$airtempchopperX)
  outdata$fuzzsurftempNX <- evalbyeid(43051, outdata$airtempchopperNX) 
  
  outdata$fuzzairtempX <- evalbyeid(42995, indata$airtemp)
  outdata$fuzzairtempNX <- evalbyeid(42996, indata$airtemp)
  
  ### combine all the x/nx rows together
  outdata$fuzzprecip <-
    if_else(
      indata$xeric == 1,
      outdata$fuzzprecipX,
      outdata$fuzzprecipNX
    )
  
  outdata$fuzzsurftemp <-
    if_else(
      indata$xeric == 1,
      outdata$fuzzsurftempX,
      outdata$fuzzsurftempNX
    )
  
  outdata$fuzzairtemp <-
    if_else(
      indata$xeric == 1,
      outdata$fuzzairtempX,
      outdata$fuzzairtempNX
    )

  outdata$fuzzclimate <- 
    (outdata$fuzzheatingfactor * outdata$fuzzsurftemp + outdata$fuzzairtemp) * outdata$fuzzprecip
  
  ## 3 others 
  outdata$fuzzwrd <- evalbyeid(42987, indata$wrd, sig.scale = 2)
  outdata$fuzzwatergatheringsurface <- evalbyeid(42988, sqrt(indata$watergatheringsurface))
  outdata$fuzzom <- evalbyeid(42990, indata$om)
  
  outdata$fuzzsaturation <- evalbyeid(63800, indata$saturationmonths)
  outdata$fuzzflood <- evalbyeid(63801, indata$floodmonths) #### this is not used?
  outdata$fuzzsurfsat <- 
    do.call(pmin, c(outdata[,c('fuzzsaturation', 'fuzzflood')], na.rm = T))
  
  ## 4 combine all the subrules
  outdata$fuzzvf <- 
    sqrt(
      sqrt(outdata$fuzzwrd) * 
        sqrt(outdata$fuzzwatergatheringsurface) * 
        sqrt(outdata$fuzzom) * 
        sqrt(outdata$fuzzsurfsat) *
        outdata$fuzzclimate * 
        outdata$fuzzchem) /
    0.95
  
  # in rare cases, this can be more than 1 (highest val seen in testing was 1.026)
  # clamp it, for data quality & appearance (this should not have meaningful changes. If it does, there is an error here somewhere)
  outdata[outdata$fuzzvf > 1 & !is.na(outdata$fuzzvf), "fuzzvf"] <- 1
  
  classbreaks <- c(0, 0.1, 0.2, 0.5, 0.8, 1)
  classlabels <- c("Not suitable", "Somewhat suitable", "Moderately suitable", "Suitable", "Highly suitable")
  
  outdata$vf <- base::cut(outdata$fuzzvf, breaks = classbreaks, labels = classlabels, right = TRUE, include.lowest = T)
  
  # outdata <- 
  #   outdata %>%
  #   mutate(cokey = as.character(cokey),
  #          coiid = as.character(coiid),
  #          mukey = as.character(mukey))
  
  ## all fuzzy values are returned, as is the column for the maximum fuzzy score, and the final classification
  ## if the input was a raster stack or brick, the output will be a small 2 layer brick instead of the df
  if(rasterinput){
    vf <- r.template %>%
      setValues(factor(outdata$vf),
                levels = rev(classlabels))
    fuzzvf <- r.template %>%
      setValues(outdata$fuzzvf)
    
    outdatabrk <- brick(vf, fuzzvf)
    outdata <- outdatabrk
    names(outdata) <- c("vf", "fuzzvf")
    
  }
  
  return(outdata)
}
