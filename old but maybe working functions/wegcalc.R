# Wind Erodibility Group & Index Calculation
# Joe Brehm
# Last edited 7/28/2020

# this function is designed to perform the Wind Erodibility Group calculation on data summarized to the component level
# conversion of SSURGO-format data structures into a component-level summary is done in another function (fn.weg_prep)

# TO DO: 
# error checking. I dont think the errors are in this function though
# rework so the iteration happens internally; accept as input only the data table rather than forcing it to be called within an iterator
# several optimization points -- biggest slowdown is the if-else-then structure; either reducing nodes or switching to sth else would be biggest change

# changelog
# 7/28: renamed function from fn.weg_calc to weg_calc
# 8/28: added str_trim commands to all the case change things at the beginning

weg_calc <- function(datatable, # table with all data in it: output of weg_prep
                     returnpath = F, # options for output type. 
                     # False: return WEG&WEI. 
                     # True: also return a 3rd value, WEG with a unique identifier for which logic pathway it came through 
                     printrow = F   # print the value for 'row' at the start of the function; for tracking progress
) {
  
  sapplied <- sapply(1:nrow(datatable), function(row){
    

  
  if(printrow) print(row)
  
  # pull all values into the function environment, for cleaner code
  hzdept_r <- datatable[row,"hzdept_r"]
  wetsurface <- datatable[row,"wetsurface"]
  moistmuck <- datatable[row,"moistmuck"]
  sfragcov_r <- datatable[row,"sfragcov_r"]
  rfrags <- datatable[row,"rfrags"]
  featdept_r <- datatable[row,"featdept_r"]
  sandtotal_r <- datatable[row,"sandtotal_r"]
  siltco_r <- datatable[row,"siltco_r"]
  airtempa_r <- datatable[row,"airtempa_r"]
  rockfrags <- datatable[row,"rockfrags"]
  tex <- datatable[row,"tex"]
  dbthirdbar_r <- datatable[row,"dbthirdbar_r"]
  caco3cl <- datatable[row,"caco3cl"]
  claytotal_r <- datatable[row,"claytotal_r"]
  sandvf_r <- datatable[row,"sandvf_r"]
  
  # these are all forced into lower case only; some databases hold upper/lower/proper case and this unifies them all
  grtgrp <- str_trim(tolower(datatable[row,"grtgrp"]))
  compname <- str_trim(tolower(datatable[row,"compname"]))
  featkind2 <- str_trim(tolower(datatable[row,"featkind2"]))
  taxtemp <- str_trim(tolower(datatable[row,"taxtemp"]))
  taxord <- str_trim(tolower(datatable[row,"taxord"]))
  taxmin <- str_trim(tolower(datatable[row,"taxmin"]))
  
  # texture goes to uppercase though; shortcut taken to simplify translation from CVIR
  texture <- str_trim(toupper(datatable[row,"texture"]))
    
  
  tryCatch({
    # catch all NA calculations: there will be many, as any NA variable can end the entire function early. 
    # unfortunately, sometimes an NA error will occur despite the entry being acceptable to later if-then paths. 
    # (Theoretically. Have not looked for specific cases yet)
    
    ### ifs and thens ####
    # this whole section was translated from CVIR using search and replace. Translation errors here are unlikely. 
    # OPTIMIZATION: The if-then tree is very slow. Might be a way to reduce it to fewer nodes. Or maybe decision trees. 
    if (
      (wetsurface == 1 | moistmuck == 1 | grepl("folists", grtgrp) | sfragcov_r > 90 | rfrags==1) & 
      compname != "Water" & 
      !is.na(compname)
    ) {
      groupPath <- "8a"
      
      # Footnote 3, meets criterian 3 in the requirements for andic soil properties.
    } else if (
      (
        (grepl("ASHY", texture) & featkind2 =="andic soil properties" & !is.na(featkind2) & (featdept_r ==0 | featdept_r == hzdept_r)) | 
        (grepl("(MEDL)|(HYDR)", texture) & sandtotal_r + siltco_r >= 30) |
        (grepl("MEDL", texture) & taxtemp =="cryic" & taxord == "spodosols" & airtempa_r < 4.5)
      ) & 
      (rockfrags < 35 & sfragcov_r < 35) & 
      (tex == "vfs" | tex =="fs" | tex =="s" | tex =="cos" | tex == "lvfs" | tex =="lfs" | tex =="ls" | tex =="lcos")
    ) { 
      groupPath <- "2a"
      
    } else if (  
      (
        (grepl("ASHY", texture) & featkind2 =="andic soil properties" & !is.na(featkind2) & (featdept_r ==0 | featdept_r == hzdept_r)) |
        (grepl("(MEDL)|(HYDR)", texture) & sandtotal_r + siltco_r >= 30) |
        (grepl("MEDL", texture) & taxtemp =="cryic" & taxord == "spodosols" & airtempa_r < 4.5)
      ) & 
      (rockfrags < 15 & sfragcov_r < 15)
    ) { 
      groupPath <- "2b"
      
    } else if (
      (
        (grepl("ASHY", texture) & featkind2 =="andic soil properties" & !is.na(featkind2) & (featdept_r ==0 | featdept_r == hzdept_r)) |
        (grepl("(MEDL)|(HYDR)", texture) & sandtotal_r + siltco_r >= 30) |
        (grepl("MEDL", texture) & taxtemp =="cryic" & taxord == "spodosols" & airtempa_r < 4.5)
      ) & 
      (rockfrags < 35 & sfragcov_r < 35)  
    ) { 
      groupPath <- "3d"
      
    } else if (  
      (
        (grepl("ASHY", texture) & featkind2 =="andic soil properties" & !is.na(featkind2) & (featdept_r ==0 | featdept_r == hzdept_r)) | 
        (grepl("(MEDL)|(HYDR)", texture) & sandtotal_r + siltco_r >= 30) |
        (grepl("MEDL", texture) & taxtemp =="cryic" & taxord == "spodosols" & airtempa_r < 4.5)
      ) & 
      (
        (rockfrags >= 35 & rockfrags < 60 & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)
      ) &
      (tex == "vfs" | tex =="fs" | tex =="s" | tex =="cos" | tex == "lvfs" | tex =="lfs" | tex =="ls" | tex =="lcos")  
    ) { 
      groupPath <- "3e"
      
    } else if (
      (
        (grepl("ASHY", texture) & featkind2 =="andic soil properties" & !is.na(featkind2) & (featdept_r ==0 | featdept_r == hzdept_r)) |
        (grepl("(MEDL)|(HYDR)", texture) & sandtotal_r + siltco_r >= 30) |
        (grepl("MEDL", texture) & taxtemp =="cryic" & taxord == "spodosols" & airtempa_r < 4.5)
      ) & 
      (
        (rockfrags >= 35 & rockfrags < 60 & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)
      )  
    ) { 
      groupPath <- "5a"
      
    } else if (
      (
        (grepl("ASHY", texture) & featkind2 =="andic soil properties" & !is.na(featkind2) & (featdept_r ==0 | featdept_r == hzdept_r)) |
        (grepl("(MEDL)|(HYDR)", texture) & sandtotal_r + siltco_r >= 30) |
        (grepl("MEDL", texture) & taxtemp =="cryic" & taxord == "spodosols" & airtempa_r < 4.5)
      ) & 
      (rockfrags >= 60 | sfragcov_r >= 60) &
      (tex == "vfs" | tex =="fs" | tex =="s" | tex =="cos" | tex == "lvfs" | tex =="lfs" | tex =="ls" | tex =="lcos")  
    ) { 
      groupPath <- "6a"
      
    } else if ( 
      (
        (grepl("ASHY", texture) & featkind2 =="andic soil properties" & !is.na(featkind2) & (featdept_r ==0 | featdept_r == hzdept_r)) |
        (grepl("(MEDL)|(HYDR)", texture) & sandtotal_r + siltco_r >= 30) |
        (grepl("MEDL", texture) & taxtemp =="cryic" & taxord == "spodosols" & airtempa_r < 4.5)
      ) & 
      (rockfrags >= 60 | sfragcov_r >= 60)
    ) { 
      groupPath <- "8b" #8
      
      # Footnote 4, meets criterian 2 (but not 3) in the requirements for andic soil properties. 
    } else if (  
      (
        (!is.na(featkind2) & featkind2 =="andic soil properties" & (featdept_r ==0 | featdept_r == hzdept_r)) | 
        grepl("(MEDL)|(HYDR)", texture)
      ) & 
      dbthirdbar_r <= 0.9 & (rockfrags < 35 & sfragcov_r < 35) & 
      (tex == "vfs" | tex =="fs" | tex =="s" | tex =="cos" | tex == "lvfs" | tex =="lfs" | tex =="ls" | tex =="lcos")  
    ) { 
      groupPath <- "6b"
      
    } else if (  
      (
        (!is.na(featkind2) & featkind2 =="andic soil properties" & (featdept_r ==0 | featdept_r == hzdept_r)) | 
        grepl("(MEDL)|(HYDR)", texture)
      ) & dbthirdbar_r <= 0.9 & (rockfrags < 15 & sfragcov_r < 15)
    ) { 
      groupPath <- "6c"
      
    } else if (
      (
        (!is.na(featkind2) & featkind2 =="andic soil properties" & (featdept_r ==0 | featdept_r == hzdept_r)) | 
        grepl("(MEDL)|(HYDR)", texture)
      ) & dbthirdbar_r <= 0.9 & (rockfrags < 35 & sfragcov_r < 35)
    ) { 
      groupPath <- "7a"
      
    } else if (
      (
        (!is.na(featkind2) & featkind2 =="andic soil properties" & (featdept_r ==0 | featdept_r == hzdept_r)) | 
        grepl("(MEDL)|(HYDR)", texture)
      ) & dbthirdbar_r <= 0.9 & 
      (
        (rockfrags >= 35 & rockfrags < 60 & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)
      ) & 
      (tex == "vfs" | tex =="fs" | tex =="s" | tex =="cos" | tex == "lvfs" | tex =="lfs" | tex =="ls" | tex =="lcos")  
    ) { 
      groupPath <- "7b"
      
    } else if ( 
      (
        (!is.na(featkind2) & featkind2 =="andic soil properties" & (featdept_r ==0 | featdept_r == hzdept_r)) | 
        grepl("(MEDL)|(HYDR)", texture)) & dbthirdbar_r <= 0.9  
    ) { 
      groupPath <- "8c"
      
    } else if (  
      (rockfrags < 35 & sfragcov_r < 35) & 
      (tex == "vfs" | tex =="fs" | tex =="s" | tex =="cos") 
    ) { 
      groupPath <- ("1a")
      
    } else if (  
      (
        (rockfrags >= 35 & rockfrags < 60 & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)
      ) & 
      (tex == "vfs" | tex =="fs" | tex =="s" | tex =="cos")  
    ) { 
      groupPath <- ("2c")
      
    } else if ( 
      (rockfrags >= 60 | sfragcov_r >= 60) & 
      (tex == "vfs" | tex =="fs" | tex =="s" | tex =="cos")  
    ) { 
      groupPath <- "5b"
      
    } else if (
      (rockfrags < 15 & sfragcov_r < 15) & 
      (
        (
          (tex =="l" | tex =="sil") & 
          caco3cl < 5 & claytotal_r >= 20
        ) | 
        (
          (tex =="cl" | tex =="sicl") & 
          caco3cl < 5 & claytotal_r <= 35
        ) | 
        (tex =="sil" & 
         (taxmin =="sesquic" | taxmin =="parasesquic" | taxmin =="ferritic" | taxmin =="ferruginous" | taxmin =="kaolinitic")
        )
      )  
    ) { 
      groupPath <- "6d"
      
    } else if (  
      (
        (rockfrags >= 15 & rockfrags < 35 & sfragcov_r < 35) | 
        (sfragcov_r >= 15 & sfragcov_r < 35 & rockfrags < 35)
      ) & 
      (
        (
          (tex =="l" | tex =="sil") & 
          caco3cl < 5 & claytotal_r >= 20
        ) | 
        (
          (tex =="cl" | tex =="sicl") & 
          caco3cl < 5 & claytotal_r <= 35
        ) | 
        (tex =="sil" & 
         (taxmin =="sesquic" | taxmin =="parasesquic" | taxmin =="ferritic" | taxmin =="ferruginous" | taxmin =="kaolinitic")
        )
      )  
    ) {
      groupPath <- "7c"
      
    } else if (  
      (rockfrags >= 35 | sfragcov_r >= 35) & 
      (
        (
          (tex =="l" | tex =="sil") & caco3cl < 5 & claytotal_r >= 20) | 
        (
          (tex =="cl" | tex =="sicl") & caco3cl < 5 & claytotal_r <= 35) | 
        (tex =="sil" & 
         (taxmin =="sesquic" | taxmin =="parasesquic" | taxmin =="ferritic" | taxmin =="ferruginous" | taxmin =="kaolinitic")
        )
      )  
    ) {
      groupPath <- "8d"
      
    } else if (  
      (rockfrags < 35 & sfragcov_r < 35) & 
      (tex == "lvfs" | tex =="lfs" | tex =="ls" | tex =="lcos")  
    ) { 
      groupPath <- ("2d")
      
    } else if (  
      (rockfrags < 15 & sfragcov_r < 15) & 
      (
        (
          (tex =="vfsl" | tex =="sil") & 
          claytotal_r <= 5 & sandvf_r <= 25
        ) | 
        (tex =="sil" & claytotal_r < 5 & sandvf_r > 25) | 
        (grepl("MUCK", texture)
        )
      )  
    ) { 
      groupPath <- ("2e")
      
    } else if (  
      (
        (rockfrags >= 15 & rockfrags < 35 & sfragcov_r < 35) | 
        (sfragcov_r >= 15 & sfragcov_r < 35 & rockfrags < 35)
      ) & 
      (
        (
          (tex =="vfsl" | tex =="sil") & 
          claytotal_r <= 5 & sandvf_r <= 25
        ) | 
        (tex =="sil" & claytotal_r < 5 & sandvf_r > 25) |
        (grepl("MUCK", texture)
        )
      )  
    ) { 
      groupPath <- ("3a")
      
    } else if (  
      (
        (rockfrags >= 35 & rockfrags < 60 & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)
      ) & 
      (tex == "lvfs" | tex =="lfs" | tex =="ls" | tex =="lcos")  
    ) { 
      groupPath <- ("3b")
      
    } else if (  
      (rockfrags >= 60 | sfragcov_r >= 60) & 
      (tex == "lvfs" | tex =="lfs" | tex =="ls" | tex =="lcos")  
    ) { 
      groupPath <- "6f"
      
    } else if (
      (
        (rockfrags >= 35 & rockfrags < 60  & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)
      ) & 
      (
        (
          (tex =="vfsl" | tex =="sil") & 
          claytotal_r <= 5 & sandvf_r <= 25
        ) | 
        (
          tex =="sil" & claytotal_r < 5 & sandvf_r > 25
        ) | 
        grepl("MUCK", texture)
      )  
    ) {
      groupPath <- ("4a")
      
    } else if (  
      (rockfrags >= 60 | sfragcov_r >= 60) & 
      (
        (
          (tex =="vfsl" | tex =="sil") & 
          claytotal_r <= 5 & sandvf_r <= 25
        ) | 
        (
          tex =="sil" & claytotal_r < 5 & sandvf_r > 25
        ) | 
        grepl("MUCK", texture)
      )  
    ) { 
      groupPath <- "8e"
      
    } else if (  
      (rockfrags < 15 & sfragcov_r < 15) & 
      (
        (tex == "vfsl" | tex =="fsl" | tex =="sl" | tex =="cosl") | 
        (tex =="sil" & caco3cl < 5 & claytotal_r >= 5 & claytotal_r < 12 & sandvf_r >= 20 & sandvf_r < 50)
      )  
    ) {
      groupPath <- ("3c")
      
    } else if (  
      (
        (rockfrags >= 15 & rockfrags < 35 & sfragcov_r < 35) | 
        (sfragcov_r >= 15 & sfragcov_r < 35 & rockfrags < 35)
      ) & 
      (
        (tex == "vfsl" | tex =="fsl" | tex =="sl" | tex =="cosl") 
        | (tex =="sil" & caco3cl < 5 & claytotal_r >= 5 & claytotal_r < 12 & sandvf_r >= 20 & sandvf_r < 50)
      )  
    ) { 
      groupPath <- "5c"
      
    } else if (  
      (
        (rockfrags >= 35 & rockfrags < 60 & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)
      ) & 
      (
        (tex == "vfsl" | tex =="fsl" | tex =="sl" | tex =="cosl") | 
        (tex =="sil" & caco3cl < 5 & claytotal_r >= 5 & claytotal_r < 12 & sandvf_r >= 20 & sandvf_r < 50)
      )
    ) { 
      groupPath <- "6g"
      
    } else if (  
      (rockfrags >= 60 | sfragcov_r >= 60) & 
      (
        (tex == "vfsl" | tex =="fsl" | tex =="sl" | tex =="cosl") | 
        (tex =="sil" & caco3cl < 5 & claytotal_r >= 5 & claytotal_r < 12 & sandvf_r >= 20 & sandvf_r < 50)
      )  
    ) { 
      groupPath <- "8f"
      
    } else if (  
      (rockfrags < 15 & sfragcov_r < 15) & 
      (
        (tex =="si" & caco3cl < 5) | 
        (
          (
            (tex =="sic" | tex =="sicl" | tex =="c") & caco3cl < 5
          ) & 
          (taxmin =="sesquic" | taxmin =="parasesquic" | taxmin =="ferritic" | taxmin =="ferruginous" | taxmin =="kaolinitic")
          & (taxord =="oxisols" | taxord =="ultisols")
        ) | 
        grepl("PEAT", texture)
      )  ) { 
      groupPath <- "7d"
      
    } else if (  
      (rockfrags >= 15 | sfragcov_r >= 15) & 
      (
        (tex =="si" & caco3cl < 5) | 
        (
          (
            (tex =="sic" | tex =="sicl" | tex =="c") & caco3cl < 5) & 
          (taxmin =="sesquic" | taxmin =="parasesquic" | taxmin =="ferritic" | taxmin =="ferruginous" | taxmin =="kaolinitic")
          & (taxord =="oxisols" | taxord =="ultisols")
        ) | 
        grepl("PEAT", texture)
      )  
    ) { 
      groupPath <- "8g"
      
    } else if (  
      (rockfrags < 15 & sfragcov_r < 15) & 
      (
        (tex =="c" | tex =="sic" | 
         (tex =="cl" & caco3cl < 5 & claytotal_r > 35)
        ) | 
        (
          tex =="sicl" & caco3cl < 5 & claytotal_r > 35 & 
          taxmin !="sesquic" & taxmin !="parasesquic" & taxmin !="ferritic" & taxmin !="ferruginous" & taxmin !="kaolinitic")
      )  
    ) { 
      groupPath <- "4b"
      
    } else if (
      (
        (rockfrags >= 15 & rockfrags < 35 & sfragcov_r < 35) | 
        (sfragcov_r >= 15 & sfragcov_r < 35 & rockfrags < 35)
      ) & 
      (
        (tex =="c" | tex =="sic" | (tex =="cl" & caco3cl < 5 & claytotal_r > 35)
        ) | 
        (tex =="sicl" & caco3cl < 5 & claytotal_r > 35 & taxmin !="sesquic" & taxmin !="parasesquic" & taxmin !="ferritic" & 
         taxmin !="ferruginous" & taxmin !="kaolinitic")
      )
    ) { 
      groupPath <- "5d"
      
    } else if (  
      (
        (rockfrags >= 35 & rockfrags < 60 & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)) & 
      (
        (tex =="c" | tex =="sic" | (tex =="cl" & caco3cl < 5 & claytotal_r > 35)
        ) | 
        (tex =="sicl" & caco3cl < 5 & claytotal_r > 35 & taxmin !="sesquic" & taxmin !="parasesquic" & taxmin !="ferritic" & 
         taxmin !="ferruginous" & taxmin !="kaolinitic")
      )
    ) { 
      groupPath <- "6e"
      
    } else if (
      (rockfrags >= 60 | sfragcov_r >= 60) & 
      (
        (tex =="c" | tex =="sic" | (tex =="cl" & caco3cl < 5 & claytotal_r > 35)
        ) | 
        (tex =="sicl" & caco3cl < 5 & claytotal_r > 35 & taxmin !="sesquic" & taxmin !="parasesquic"& taxmin !="ferritic" & 
         taxmin !="ferruginous" & taxmin !="kaolinitic")
      )
    ) { 
      groupPath <- "8h"
      
    } else if (  
      (rockfrags < 15 & sfragcov_r < 15) & 
      (tex =="l" | tex =="sil" | tex =="si" | tex =="cl" | tex =="sc" | tex =="scl" | tex =="sicl") & caco3cl >=5  
    ) { 
      groupPath <- "4L"
      
    } else if (
      (
        (rockfrags >= 15 & rockfrags < 35 & sfragcov_r < 35) | 
        (sfragcov_r >= 15 & sfragcov_r < 35 & rockfrags < 35)
      ) & 
      (tex =="l" | tex =="sil" | tex =="si" | tex =="cl" | tex =="sc" | tex =="scl" | tex =="sicl") & caco3cl >=5  
    ) { 
      groupPath <- "5e"
      
    } else if (  
      (
        (rockfrags >= 35 & rockfrags < 60 & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)
      ) & 
      (tex =="l" | tex =="sil" | tex =="si" | tex =="cl" | tex =="sc" | tex =="scl" | tex =="sicl") & caco3cl >=5  
    ) { 
      groupPath <- "6h"
      
    } else if (
      (rockfrags >= 60 | sfragcov_r >= 60) & 
      (tex =="l" | tex =="sil" | tex =="si" | tex =="cl" | tex =="sc" | tex =="scl" | tex =="sicl") & 
      caco3cl >=5  
    ) { 
      groupPath <- "8i"
      
    } else if (  
      (rockfrags < 15 & sfragcov_r < 15) & 
      (
        (tex =="l" & caco3cl < 5 & claytotal_r < 20) | 
        (tex =="sil" & caco3cl < 5 & claytotal_r >= 5 & claytotal_r < 20) | 
        (
          (tex =="scl" | tex =="sc") & caco3cl < 5) | 
        grepl("MPT", texture)
        
      )  
    ) { 
      groupPath <- "5f"
      
    } else if (
      (
        (rockfrags >= 15 & rockfrags < 35 & sfragcov_r < 35) | 
        (sfragcov_r >= 15 & sfragcov_r < 35 & rockfrags < 35)
      ) & 
      (
        (tex =="l" & caco3cl < 5 & claytotal_r < 20) | 
        (tex =="sil" & caco3cl < 5 & claytotal_r >= 5 & claytotal_r < 20) | 
        (
          (tex =="scl" | tex =="sc") & caco3cl < 5) | 
        grepl("MPT", texture)
      )
    ) { 
      groupPath <- "6i"
      
    } else if (  
      (
        (rockfrags >= 35 & rockfrags < 60 & sfragcov_r < 60) | 
        (sfragcov_r >= 35 & sfragcov_r < 60 & rockfrags < 60)
      ) & 
      (
        (tex =="l" & caco3cl < 5 & claytotal_r < 20) | 
        (tex =="sil" & caco3cl < 5 & claytotal_r >= 5 & claytotal_r < 20) | 
        (
          (tex =="scl" | tex =="sc") & caco3cl < 5) | 
        grepl("MPT", texture)
      )
    ) {
      groupPath <- "7e"
      
    } else if (  
      (rockfrags >= 60 | sfragcov_r >= 60) & 
      (
        (tex =="l" & caco3cl < 5 & claytotal_r < 20) | 
        (tex =="sil" & caco3cl < 5 & claytotal_r >= 5 & claytotal_r < 20) | 
        (
          (tex =="scl" | tex =="sc") & caco3cl < 5) | 
        grepl("MPT", texture)
      )
    ) { 
      groupPath <- "8j"
    } else {
      groupPath <- "NA: Not accepted by any path"
    }
    
    ### end ifs and thens ####
    
    ### post processing ####
    # at this point, groupPath contains the specific pathway code that it followed (eg 1a)
    # some cases need to change this: create "group", containing just the WEG group code (eg 1)
    if(grepl("NA", groupPath)){
      group <- NA
    } else if(groupPath == "4L") {
      group <- "4L"
    } else {
      group <- substr(groupPath, 0, 1)
    }
    
    # overwrite group in vitric cases. REMEMBER THAT THE CVIR CODE USES 1-9, not 1-4, 4L, and 5-8
    if(group != "1" & datatable[row,"vitri"] == 1) {
      if(group == "4L"){
        groupPath <- "3vitri"
        group <- "3"
      } else if (group == "5"){
        groupPath <- "4vitri"
        group <- "4"
      } else {
        groupPath <- paste0(as.numeric(group) - 1, "SubtrVitri")
        group <- paste0(as.numeric(group) - 1)
      }
    }
    
    # calculate wind erodibility index
    if(group == "1"){
      if(tex == "vfs") {
        wei <- "310"
      } else if(tex == "fs") {
        wei <- "250"
      } else if(tex == "cos") {
        if(rockfrags >= 15 | sfragcov_r >= 15) {
          wei <- "160"
        } else {
          wei <- "180"
        }
      } else {
        wei <- "220"
      }
    }
    if(group == "2"){
      wei <- "134"
    }
    if(group %in% c("3", "4", "4L")){
      wei <- "86"
    }
    if(group == "5"){
      wei <- "56"
    }
    if(group == "6"){
      wei <- "48"
    }
    if(group == "7"){
      wei <- "38"
    }
    if(group == "8"){
      wei <- "0"
    }
    
    ### Exit! 
    out <- c(datatable[row,"cokey"],
             wegcalc = as.character(group),
             weicalc = as.character(wei))
    if(returnpath) out$wegpath <- groupPath
    
    return(out)
    
    ### back end of the trycatch function: return generic errors
  }, error = function(err){
    group <- NA
    wei <- NA
    
    if(grepl("missing value", paste(err))) {
      err <- "missing data in source"
    }
    
    groupPath <- paste("NA:", err)
    
    out <- c(datatable[row,"cokey"],
                      wegcalc = as.character(group),
                      weicalc = as.character(wei))
    if(returnpath) out$wegpath <- groupPath
    return(out)
  }
  )
  
  }
  
  )
  
  if(returnpath) {ncol <- 4} else {ncol <- 3}
  
  formatted <- as.data.frame(
    matrix(unlist(sapplied),
                      ncol = ncol,
                      byrow = T))
  colnames(formatted) <- rownames(sapplied)
  
  
  return(formatted)
  
}

