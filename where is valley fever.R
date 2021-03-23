setwd("E:/NMSU/interp-engine-personal")

dbcrosswalk <-
  read.csv("dbcrosswalk\\dbcrosswalk_inner_AK-AZ-CT-NE-UT-NM.csv")

source('Functions/local-functions.R')
load('Engine Guts/cached-NASIS-data.Rda')


evals$evalname[grepl(x = evals$evalname, pattern = "Valley", ignore.case = T)]

# [1] "Valley Fever Mean Annual Precipitation, 125-400mm"      "Soil Reaction Within 30cm (Valley Fever)"              
# [3] "Valley Fever Mean Xeric Precipitation, 125-400mm"       "Valley Fever Mean Non-Xeric Precipitation, 125-400mm"  
# [5] "Valley Fever EC WTD_AVG 0-30cm"                         "Valley Fever Heat Loading Slope"                       
# [7] "Slope Aspect Factor for Valley Fever"                   "Valley Fever Mean Xeric Precip, 125-400mm, 2 C Warming"
# [9] "Valley Fever Mean Non-Xeric Precip, 125-400mm, 2C Warm"

rules$rulename[grepl(x = rules$rulename, pattern = "Valley", ignore.case = T)]
# [1] "*Storie Factor X (erosion in valley)"                         "CPI - Grass Hay, IRR - Klamath Valleys and Basins (OR)"      
# [3] "CPI - Grass Hay, NIRR - Klamath Valleys and Basins (OR)"      "CPI - Alfalfa Hay, IRR - Klamath Valley and Basins (OR)"     
# [5] "Mean Annual  Precipitation for Valley Fever"                  "Mean Annual  Air Temperature for Valley Fever"               
# [7] "Climatic Subrule for Valley Fever"                            "Chemical Subrule for Valley Fever"                           
# [9] "Valley Fever EC WTD_AVG 0 to 30 cm, 8-16mmhos"                "Slope Heating Factor Subrule for Valley Fever"               
# [11] "Albedo Factor Valley Fever"                                   "Slope Gradient Valley Fever Heat Loading"                    
# [13] "Slope Aspect Factor valley Fever"                             "Xeric Air Temperature, Valley Fever"                         
# [15] "Xeric Precipitation, Valley Fever"                            "Non-Xeric Air Temperature, Valley Fever"                     
# [17] "Non-Xeric Precipitation, Valley Fever"                        "Xeric Surface Temperature Limiter, Valley Fever"             
# [19] "Non-Xeric Air Temperature Limiter, Valley Fever"              "CPI - Grass-Legume Hay, NIRR - Puget Sound Valleys (WA) v1"  
# [21] "CPI - Grass-Legume Hay, NIRR - Puget Sound Valleys (WA) v2"   "Climatic Subrule for Valley Fever, 2 C Warming"              
# [23] "Xeric Precipitation, Valley Fever, 2 C Warming"               "Xeric Surface Temperature Limiter, Valley Fever, 2C Warming" 
# [25] "Xeric Air Temperature, Valley Fever, 2C Warming"              "Non-Xeric Precipitation, Valley Fever, 2C Warming"           
# [27] "Non-Xeric Air Temperature Limiter, Valley Fever, 2C Warm"     "Non-Xeric Air Temperature, Valley Fever, 2C Warm"            
# [29] "CPI - Small Grains, NIRR - Willamette Valley (OR)"            "CPI - Small Grains, NIRR - Willamette Valley (OR) v2A wti"   
# [31] "CPI - Small Grains, NIRR - Willamette Valley (OR) v2B drainc" "CPI - Small Grains, NIRR - Willamette Valley (OR) v3"        
# [33] "Valley Fever Near Surface Saturation"                         "Valley Fever" 

subset(rules, 
       rulename == "Valley Fever")

subset(rules,
       rulename == "Soil Habitat for Saprophyte Stage of Coccidioides")
