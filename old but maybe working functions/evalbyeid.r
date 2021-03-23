### DEPRECATED!  MOVED TO LOCAL-FUNCTIONS

# require(plyr)
# require(tidyverse) 
# require(data.tree)
# require(XML)
# require(soilDB)
# require(digest)
# 
# # evaluation by the eid code
# evalbyeid <- 
#   function(
#     eid, # eval id -- nb, not rule id or property id. Evals only. 
#     d
#   ) {
#     e = evals[evals$evaliid == eid,]
#     f = extractEvalCurve(e)
#     outdata = f(d)
#     return(outdata)
#   }
