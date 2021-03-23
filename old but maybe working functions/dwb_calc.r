require(plyr)
require(tidyverse) 
require(data.tree)
require(XML)
require(soilDB)
require(digest)
require(raster)

dwb_calc <- function(indata){
  
  # this works on a data frame. 
  # if a raster stack or brick is passed as input, it will convert to df
  
  rasterinput <- class(indata)[1] %in% c("RasterStack", "RasterBrick")
  #print(rasterinput)
  if(rasterinput) {
    r.template <- indata[[1]] # save the first one as a template to create an out raster later
    indata <- as.data.frame(indata)
  }
  
  outdata <- indata[,0] # this makes an empty data frame with the correct num of rows
  
  # 1 - depth to permafrost
  outdata$fuzzpermdepth <-
    pmax(
      evalbyeid(10356, indata$permdepth) %>% replace_na(0), ### case 1: fuzzy logic eval
      indata$pftex %>% as.integer() %>% replace_na(0) # T when there is a pf code in either texinlieu or texmod      
    )
  
  # 2 - ponding duration
  outdata$fuzzpond <- 
    indata$ponding %>% as.integer() %>% replace_na(0)

    # 3 - slope
  outdata$fuzzslope <- evalbyeid(10125, indata$slope_r) #%>% replace_na(0) # null goes to NR
  
  # 4 - subsidence(cm)
  # this is secretly a crisp eval
  outdata$fuzzsubsidence <- as.numeric(as.numeric(indata$totalsub_r) >= 30) %>% replace_na(0)
  
  # 5 - flooding frequency
  outdata$fuzzfloodlim <- 
    indata$flooding %>% as.integer() %>% replace_na(0)
  
  # 6 - depth to water table
  outdata$fuzzwt <- evalbyeid(299, indata$wt) %>% replace_na(0)
  
  # 7 - shrink-swell
  outdata$fuzzshrinkswell <- evalbyeid(18502, indata$lep) %>% replace_na(0)
  
  # 8 - om content class of the last layer above bedrock / deepest layer
  outdata$fuzzomlim <- 
    indata$organicsoil %>% as.integer() %>% replace_na(0)
    
  # 9 - depth to bedrock (hard)
  outdata$fuzzdepbrockhard <- evalbyeid(18503, indata$depbrockhard) %>% replace_na(0)
  
  # 10 - depth to bedrock (soft)
  outdata$fuzzdepbrocksoft <- evalbyeid(18504, indata$depbrocksoft) %>% replace_na(0)
  
  # 11 - large stone content
  outdata$fuzzlgstone <- evalbyeid(267, indata$fragvol_wmn) # %>% replace_na(0) #### null goes to not rated
  
  # 12 - depth to cemented pan (thick)
  outdata$fuzzdepcementthick <- evalbyeid(18505, indata$depcementthick) %>% replace_na(0) ### the fuzzy space here is not whats in the notes
  #outdata$fuzzdepcementthick[indata$noncemented] <- 0
  
  # 13 - depth to cemented pan (thin)
  outdata$fuzzdepcementthin <- evalbyeid(18501, indata$depcementthin) %>% replace_na(0)
  #outdata$fuzzdepcementthin[indata$noncemented] <- 0
  
  # 14 - unstable fill
  outdata$fuzzunstable <- 
    indata$unstablefill %>% as.integer() %>% replace_na(0)
    
  # 15 - subsidence due to gypsum
  outdata$fuzzgypsum <- evalbyeid(16254, indata$gypsum) # %>% replace_na(0) ## null to not rated
  
  # 16 impaction
  outdata$fuzzimpaction <- 
    indata$impaction %>% as.integer() %>% replace_na(0)
    
  # 17 drainage class
  outdata$fuzzdrainage <- (indata$drainageclass != 1) %>% as.integer() %>% replace_na(0)
  
  ### aggregate, returning the highest fuzzy value (ie, most limiting variable) and classifying based on it
  firstcol <- which(colnames(outdata) == "fuzzpermdepth")
  lastcol <- which(colnames(outdata) == "fuzzdrainage")
  
  outdata$maxfuzz <- do.call(pmax, c(outdata[,firstcol:lastcol], na.rm = T))
  
  outdata$dwb <- 
    if_else(outdata$maxfuzz == 1, 
            "Very limited",
            if_else(outdata$maxfuzz == 0, 
                    "Not limited",
                    "Somewhat limited")) %>% as.factor()
  
  ## all fuzzy values are returned, as is the column for the maximum fuzzy score, and the final DWB classification
  
  ## if the input was a raster stack or brick, the output will be a small 2 layer brick instead of the df
  if(rasterinput){
    dwb <- r.template %>% 
      setValues(factor(outdata$dwb), 
                levels = rev(c("Not limited", "Somewhat limited", "Very limited")))
    fuzzdwb <- r.template %>%
      setValues(outdata$maxfuzz)
    
    outdata <- brick(dwb, fuzzdwb)
    names(outdata) <- c("dwb", "maxfuzz")
  }
  
  
  return(outdata)
}

