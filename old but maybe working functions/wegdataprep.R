# Data Processing for Wind Erodibility Group & Index Calculation
# Joe Brehm
# Last edited 7/28/2020

# this function is designed to aggregate soils data from SSURGO-like format into component level summaries
# output data is formatted such that it can be fed into the WEG calc function (fn.weg_calc)

# TO DO: 
# bug hunt. I am fairly certain the errors are in this function. Likely candidates are joins 5, and 6
# work on ensuring it can handle input source variation: small format differences may cause errors
# several optimization points, marked in comments
# overwrite output for Badlands / Rock Outcrop / similar?

# updates:
# 07/28: Added option to output intermediate tables
#        Renamed function from fn.weg_prep to weg_prep    
# 08/03: Improved NA handling -- fewer components will get null values now
# 08/21: All joins involving tables subordinate to component (ie, not legend or map unit but everything else) changed from inner to left -- no change in accuracy or data loss
# 08/27: Added str_trim commands whenever a string is used or created

# wrap all the joins into a function
weg_prep <- function(tbl.chorizon = ls.tables$chorizon,
                    tbl.component = ls.tables$component,
                    tbl.chtexturegrp = ls.tables$chtexturegrp,
                    tbl.chtexture = ls.tables$chtexture,
                    tbl.cosurffrags = ls.tables$cosurffrags,
                    tbl.cotaxfmmin = ls.tables$cotaxfmmin,
                    tbl.chfrags = ls.tables$chfrags,
                    tbl.codiagfeatures = ls.tables$codiagfeatures,
                    tbl.comonth = ls.tables$comonth,
                    tbl.cosoilmoist = ls.tables$cosoilmoist,
                    tbl.mapunit = ls.tables$mapunit,
                    tbl.legend = ls.tables$legend,
                    verbose = F,
                    intermediates = F){
  
  require(dplyr)
  
  ### CVIR code is retained in commented lines, for reference
  
  ls.SummaryTables <- list() # this list object will hold the joined data intermediate steps
  
  # 1.1 Aggregation Table: Texture join ####
  
  # EXEC SQL 
  # SELECT coiid, compkind, localphase, taxgrtgroup, taxorder, taxtempregime, taxsuborder, taxsubgrp, airtempa_r, hzdept_r, chiid, 
  # texcl, texture, hzdepb_r, claytotal_r, sandtotal_r, sandvf_r, caco3_r, dbthirdbar_r, chtexturegrp.rv_indicator, siltco_r, compname,
  # sandvc_r, sandco_r, sandmed_r, sandfine_r, silttotal_r 
  # FROM component, chorizon, chtexturegrp, chtexture
  # WHERE join component to chorizon and join chorizon to chtexturegrp and join chtexturegrp to chtexture and chtexturegrp.rv_indicator=1
  # AND texture not matches "*MPM*" AND texture not matches "*SPM*" AND texture not matches "*HPM*";
  # SORT BY coiid, hzdept_r
  # AGGREGATE COLUMN hzdept_r FIRST, hzdepb_r FIRST, texcl FIRST, texture FIRST, claytotal_r FIRST, sandtotal_r FIRST, 
  # sandvf_r FIRST, siltco_r FIRST, caco3_r FIRST, dbthirdbar_r FIRST,
  # chtexturegrp.rv_indicator first, sandvc_r first, sandco_r first, sandmed_r first, sandfine_r first, silttotal_r first.
  
  tbl.join <- 
    inner_join(tbl.legend, 
               tbl.mapunit,
               by = "lkey") %>%
    inner_join(tbl.component, 
               by = "mukey") %>%
    left_join(tbl.chorizon, 
              by = "cokey") %>%
    left_join(tbl.chtexturegrp, 
              by = "chkey") %>%
    left_join(tbl.chtexture, 
              by = "chtgkey") %>%
    dplyr::select(cokey, 
                  compkind, 
                  localphase, 
                  taxgrtgroup, 
                  taxorder, 
                  taxtempregime, 
                  taxsuborder, 
                  taxsubgrp, 
                  airtempa_r,
                  hzdept_r,
                  chkey, 
                  texcl,
                  texture, 
                  hzdepb_r, 
                  claytotal_r, 
                  sandtotal_r, 
                  sandvf_r, 
                  caco3_r, 
                  dbthirdbar_r, 
                  rvindicator, 
                  siltco_r, 
                  compname, 
                  sandvc_r,
                  sandco_r, 
                  sandmed_r, 
                  sandfine_r, 
                  silttotal_r,
                  areasymbol) %>%
    mutate_if(is.factor, forcats::fct_explicit_na) %>%
    filter(str_trim(tolower(rvindicator)) == "yes") %>%
    filter(!grepl("[HSM]PM", texture))  %>%
    arrange(compname, hzdept_r) %>%
    group_by(cokey) %>%
    summarise(.groups = "drop",
              hzdept_r = first(hzdept_r),
              airtempa_r = first(airtempa_r), 
              hzdepb_r = first(hzdepb_r),
              texcl = first(texcl),
              texture = first(texture),
              claytotal_r = first(claytotal_r),
              sandtotal_r = first(sandtotal_r),
              sandvf_r = first(sandvf_r),
              siltco_r = first(siltco_r),
              caco3_r = first(caco3_r),
              dbthirdbar_r = first(dbthirdbar_r),
              rv_indicator = first(rvindicator), # renamed to match nasis. May need to be flexible with different data sources
              sandvc_r = first(sandvc_r),
              sandco_r = first(sandco_r),
              sandmed_r = first(sandmed_r),
              sandfine_r = first(sandfine_r),
              silttotal_r = first(silttotal_r),
              compkind = first(compkind),
              compname = first(compname),
              localphase = first(localphase),
              taxgrtgroup = first(taxgrtgroup),
              taxorder = first(taxorder),
              taxsubgrp = first(taxsubgrp),
              taxsuborder = first(taxsuborder),
              taxtempregime = first(taxtempregime),
              areasymbol = first(areasymbol),
    ) %>%
    mutate_if(is.character, replace_na, replace = "(Missing)") %>%
    mutate_if(is.character, str_trim) %>%
    mutate_if(is.factor, fct_explicit_na)
  
  ls.SummaryTables[[1]] <- tbl.join
  
  if(verbose) print("Section 1.1 complete")
  
  # 1.2 Aggregation Table: surface fragments ####
  # # EXEC SQL 
  # # SELECT sfragcov_r
  # # FROM component, cosurffrags
  # # WHERE join component to cosurffrags;
  # # AGGREGATE COLUMN sfragcov_r SUM.
  
  ls.SummaryTables[[2]] <- 
    left_join(tbl.component, 
              tbl.cosurffrags,
              by = "cokey") %>%
    dplyr::select(cokey, 
                  sfragcov_r) %>%
    group_by(cokey) %>%
    summarise(.groups = "drop",
              sfragcov_r = sum(sfragcov_r)) %>%
    mutate_if(is.character, replace_na, replace ="(Missing)") %>%
    mutate_if(is.character, str_trim) %>%
    mutate_if(is.factor, fct_explicit_na)
  
  
  if(verbose) print("Section 1.2 complete")
  
  
  # 1.3 Aggregation Table: taxminalogy 
  ##  CVIR USES MINORDER -- SELECTING THE TOP MINERALOGY ORDER WHEN MORE ARE PRESENT
  ##  MINORDER IS NOT PRESENT OUTSIDE NASIS. USED HORIZON DEPTH INSTEAD ##
  
  # # SELECT taxminalogy, minorder
  # # FROM component, cotaxfmmin
  # # WHERE join component to cotaxfmmin;
  # # SORT by minorder DESC
  # # AGGREGATE COLUMN taxminalogy FIRST.
  
  ls.SummaryTables[[3]] <- 
    left_join(tbl.component, 
              tbl.cotaxfmmin, by = "cokey") %>%
    mutate_if(is.factor, forcats::fct_explicit_na) %>%
    group_by(cokey) %>%
    summarise(.groups = "drop",
              taxminalogy = first(taxminalogy)) %>%
    mutate_if(is.character, replace_na, replace ="(Missing)") %>%
    mutate_if(is.character, str_trim) %>%
    mutate_if(is.factor, fct_explicit_na)
  
  if(verbose) print("Section 1.3 complete")
  
  
  # 1.4 Aggregation Table: andic feature depth ####
  # # SELECT featkind, featdept_r
  # # FROM component, codiagfeatures
  # # WHERE join component to codiagfeatures
  # # AND (featkind = "andic soil properties" OR featkind is null);
  # # SORT by featdept_r
  # # AGGREGATE COLUMN featkind FIRST, featdept_r FIRST.
  
  ls.SummaryTables[[4]] <- 
    left_join(tbl.component, 
              tbl.codiagfeatures,
              by = "cokey") %>%
    dplyr::select(cokey, 
                  featkind, 
                  featdept_r) %>%  
    filter(str_trim(tolower(featkind)) == "andic soil properties" | is.na(featkind) | featkind == "(Missing)") %>%
    arrange(featdept_r) %>%
    group_by(cokey) %>%
    summarise(.groups = "drop",
              featkind = first(featkind),
              featdept_r = first(featdept_r)) %>%
    mutate_if(is.character, replace_na, replace ="(Missing)") %>%
    mutate_if(is.character, str_trim) %>%
    mutate_if(is.factor, fct_explicit_na)
  
  
  
  if(verbose) print("Section 1.4 complete")
  
  # 1.5 Aggregation Table: fragment volume ####
  # # SELECT coiid cd, fragvol_r, hzdept_r hzdpt 
  # # FROM component, chorizon, outer chfrags 
  # # WHERE JOIN component to chorizon AND JOIN chorizon to chfrags
  # # AND (fragkind not in ("wood", "logs and stumps") OR fragkind is null);
  # # SORT by hzdpt
  # # AGGREGATE COLUMN hzdpt NONE, fragvol_r NONE. 
  
  # from further down in the cvir...
  
  # DEFINE rocks       	REGROUP fragvol_r BY hzdpt AGGREGATE SUM.
  # ASSIGN hzdpt	   	REGROUP hzdpt BY hzdpt AGGREGATE FIRST. 
  # DEFINE rockfrags   	LOOKUP(hzdept_r, hzdpt, rocks).	
  # ASSIGN rockfrags   	IF ISNULL(rockfrags) THEN 0 ELSE rockfrags.
  
  
  ## note 8/20
  ### null = 0 in all chfrags fragvol_r after the left join
  ### always left join to chorizon!!
  
  
  ls.SummaryTables[[5]] <- 
    left_join(tbl.component, 
              tbl.chorizon,
              by = "cokey") %>%
    left_join(tbl.chfrags,
              by = "chkey") %>% ### "outer chfrags" == left outer join
    filter(str_trim(tolower(fragkind)) != "wood fragments" | is.na(fragkind) | fragkind == "(Missing)") %>%  ### fragkind codes are different between nasis and gnatsgo
    
    ## change point 821
    filter(!is.na(fragvol_r)) %>% ### NULLS ARE 0!!!
    #mutate(fragvol_r = replace_na(fragvol_r, 0)) %>%
    
    
    dplyr::select(cokey, 
                  chkey,
                  fragvol_r, 
                  hzdept_r,
                  fragkind) %>%
    group_by(cokey,
             chkey) %>%
    summarise(.groups = "drop",
              rockfrags = sum(fragvol_r),
              hzdpt = first(hzdept_r)) %>%
    arrange(cokey,
            hzdpt) %>%
    group_by(cokey) %>%
    summarise(.groups = "drop",
              rockfrags = first(rockfrags),
              hzdpt = first(hzdpt)) %>%
    group_by(cokey,
             hzdpt) %>%
    summarise(.groups = "drop",
              rockfrags = sum(rockfrags)) %>%
    arrange(cokey, hzdpt) %>%
    mutate_if(is.character, replace_na, replace ="(Missing)") %>%
    mutate_if(is.character, str_trim) %>%
    mutate_if(is.factor, fct_explicit_na)
  
  
  
  
  if(verbose) print("Section 1.5 complete")
  
  # 1.6 Aggregation Table: soil moisture ####
  # EXEC SQL
  # select soimoiststat moist, comonth.month months
  # from component, comonth, cosoilmoist
  # where join component to comonth and join comonth to cosoilmoist
  # and soimoiststat = "wet" and soimoistdept_r = 0
  # and ((taxtempregime in ("cryic", "pergelic", "gelic") and comonth.month in ("jul", "aug")) OR
  #      (taxtempregime in ("frigid", "mesic", "isofrigid") and comonth.month in ("may", "jun", "jul", "aug", "sep")) OR
  #      (taxtempregime in ("thermic", "hyperthermic") and comonth.month in ("apr", "may", "jun", "jul", "aug", "sep", "oct")) OR
  #      (taxtempregime in ("isothermic", "isohyperthermic", "isomesic")
  #        and comonth.month in ("mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov")) OR
  #      taxtempregime IS NULL);
  # SORT BY months ASC
  # AGGREGATE COLUMN months SUM.
  
  # a direct translation of the above doesn't work, because with that joining process blocks all cases with the soimoiststat & soimoisstatdept_r filter 
  # below is code from Jason, which does work
  
  # SELECT DISTINCT areasymbol, muname, mukey, compname, cokey, comppct_r, MIN(soimoistdept_r) over(partition by cokey) as min_soimoistdept_r
  # FROM (SELECT   areasymbol, muname, mapunit.mukey, compname, c.cokey, comppct_r, soimoistdept_r
  #       FROM legend
  #       INNER JOIN mapunit ON mapunit.lkey=legend.lkey AND  CASE WHEN @area_type = 2 THEN LEFT (areasymbol, 2) ELSE areasymbol END = @area
  #       INNER JOIN component AS c ON c.mukey=mapunit.mukey
  #       AND majcompflag = 'Yes'
  #       INNER JOIN comonth ON c.cokey=comonth.cokey
  #       INNER JOIN cosoilmoist ON cosoilmoist.comonthkey=comonth.comonthkey
  # ) AS s WHERE s.cokey IN
  # (SELECT  c2.cokey FROM component AS c2
  #   INNER JOIN comonth ON c2.cokey=comonth.cokey
  #   INNER JOIN cosoilmoist ON cosoilmoist.comonthkey=comonth.comonthkey
  #   AND c2.cokey=s.cokey AND soimoiststat = 'wet' AND soimoistdept_r = 0  GROUP BY c2.cokey)
  
  ### note 8/21: this can be much neater.
  
  ls.SummaryTables[[6]] <-
    # find min soil moisture depth summarized by component
    left_join(tbl.component,
              tbl.comonth,
              by = "cokey") %>%
    left_join(tbl.cosoilmoist,
              by = "comonthkey") %>%
    filter(tolower(majcompflag) == "yes") %>%
    group_by(cokey) %>%
    summarise(.groups = "drop", min_soimoistdept_r = min(soimoistdept_r))  %>%
    left_join(tbl.component, 
              by = "cokey") %>%
    left_join(tbl.comonth,
              by = "cokey") %>%  
    left_join(tbl.cosoilmoist, 
              by = "comonthkey") %>%
    mutate(taxtempregime = tolower(str_trim(taxtempregime)),
           month = str_trim(month)) %>%
    filter(str_trim(tolower(majcompflag)) == "yes") %>%
    mutate_if(is.factor, forcats::fct_explicit_na) %>% # replace all NA with "(Missing)", to prevent future errors
    filter(tolower(soimoiststat) == "wet" 
           & min_soimoistdept_r == 0) %>%
    filter(
      (taxtempregime %in% c("cryic", "pergelic", "gelic") 
       & month %in% c("July", "August")) |
        (taxtempregime %in% c("frigid", "mesic", "isofrigid") 
         & month %in% c("May", "June", "July", "August", "September")) |
        (taxtempregime %in% c("thermic", "hyperthermic") 
         & month %in% c("April", "May", "June", "July", "August", "September", "October")) |
        (taxtempregime %in% c("isothermic", "isohyperthermic", "isomesic") 
         & month %in% c("March", "April", "May", "June", "July", "August", "September", "October", "November")) |
        is.na(taxtempregime) |
        taxtempregime == "(Missing)") %>%
    group_by(cokey) %>%
    summarise(.groups = "drop",
              months = sum(monthseq)) %>%
    mutate_if(is.character, replace_na, replace ="(Missing)") %>%
    mutate_if(is.factor, fct_explicit_na)
  
  
  
  if(verbose) print("Section 1.6 complete")
  
  # 1.7 Aggregation Table: top horizon data ####
  # # EXEC SQL 
  # # SELECT hzdept_r as hzdept_r2, coiid as coiid2, 
  # # texcl as texcl2, texture as texture2, hzdepb_r as hzdepb_r2, claytotal_r as claytotal_r2, sandtotal_r as sandtotal_r2, sandvf_r as sandvf_r2, 
  # # caco3_r as caco3_r2, dbthirdbar_r as dbthirdbar_r2, chtexturegrp.rv_indicator as rv_indicator2, siltco_r as siltco_r2 
  # # FROM component, chorizon, chtexturegrp, chtexture
  # # WHERE join component to chorizon and join chorizon to chtexturegrp and join chtexturegrp to chtexture and chtexturegrp.rv_indicator=1
  # # AND hzdept_r > 0;
  # # SORT BY coiid2, hzdept_r2
  # # AGGREGATE COLUMN hzdept_r2 FIRST, hzdepb_r2 FIRST, texcl2 FIRST, texture2 FIRST, claytotal_r2 FIRST, sandtotal_r2 FIRST, 
  # # sandvf_r2 FIRST, siltco_r2 FIRST, caco3_r2 FIRST, dbthirdbar_r2 FIRST.
  
  ls.SummaryTables[[7]] <- 
    left_join(tbl.component, 
              tbl.chorizon, by = "cokey") %>%
    left_join(tbl.chtexturegrp, by = "chkey") %>%
    left_join(tbl.chtexture, by = "chtgkey") %>%
    filter(rvindicator == "Yes") %>%
    filter(hzdept_r > 0) %>%
    rename(hzdept_r2 = hzdept_r,
           cokey2 = cokey, 
           texcl2 = texcl, 
           texture2 = texture, 
           hzdepb_r2 = hzdepb_r, 
           claytotal_r2 = claytotal_r, 
           sandtotal_r2 = sandtotal_r, 
           sandvf_r2 = sandvf_r,
           caco3_r2 = caco3_r, 
           dbthirdbar_r2 = dbthirdbar_r, 
           rv_indicator2 = rvindicator, 
           siltco_r2 = siltco_r) %>%
    arrange(cokey2, hzdept_r2) %>%
    group_by(cokey2) %>%
    summarise(.groups = "drop",hzdept_r2 = first(hzdept_r2), 
              hzdepb_r2 = first(hzdepb_r2),
              texcl2 = first(texcl2), 
              texture2 = first(texture2), 
              claytotal_r2 = first(claytotal_r2), 
              sandtotal_r2 = first(sandtotal_r2), 
              sandvf_r2 = first(sandvf_r2), 
              siltco_r2 = first(siltco_r2), 
              caco3_r2 = first(caco3_r2), 
              dbthirdbar_r2 = first(dbthirdbar_r2)) %>%
    mutate_if(is.character, replace_na, replace ="(Missing)") %>%
    mutate_if(is.factor, fct_explicit_na)
  
  
  
  if(verbose) print("Section 1.7 complete")
  
  
  # 2.1 Gather data for WEG calculation #### 
  # these all come from the tables above, pull them into a single data frame to do cokey level logic pathways below
  # DEFINE tex			CODENAME(texcl).
  # DEFINE grtgrp		CODENAME(taxgrtgroup).
  # DEFINE taxmin		CODENAME(taxminalogy).
  # DEFINE suborder 	CODENAME(taxsuborder).
  # DEFINE subgrp		CODENAME(taxsubgrp).
  # DEFINE taxord		CODENAME(taxorder).
  # DEFINE taxtemp		CODENAME(taxtempregime).
  # DEFINE featkind2 	CODENAME(featkind).
  # DEFINE compkind2	CODENAME(compkind).
  # DEFINE tex2			CODENAME(texcl2).
  
  # force all cokeys to character, because defensive coding
  ls.SummaryTables[[1]]$cokey <- as.character(ls.SummaryTables[[1]]$cokey)
  ls.SummaryTables[[2]]$cokey <- as.character(ls.SummaryTables[[2]]$cokey)
  ls.SummaryTables[[3]]$cokey <- as.character(ls.SummaryTables[[3]]$cokey)
  ls.SummaryTables[[4]]$cokey <- as.character(ls.SummaryTables[[4]]$cokey)
  ls.SummaryTables[[5]]$cokey <- as.character(ls.SummaryTables[[5]]$cokey)
  ls.SummaryTables[[6]]$cokey <- as.character(ls.SummaryTables[[6]]$cokey)
  ls.SummaryTables[[7]]$cokey2 <- as.character(ls.SummaryTables[[7]]$cokey2)
  
  tbl.merge <- 
    full_join(ls.SummaryTables[[1]], 
              ls.SummaryTables[[2]],
              by = "cokey") %>%
    full_join(ls.SummaryTables[[3]],
              by = "cokey") %>%
    full_join(ls.SummaryTables[[4]],
              by = "cokey") %>%
    full_join(ls.SummaryTables[[5]],
              by = "cokey") %>%
    full_join(ls.SummaryTables[[6]],
              by = "cokey") %>%
    full_join(ls.SummaryTables[[7]],
              by = c("cokey" = "cokey2")) %>%
    rename(tex = texcl,
           grtgrp = taxgrtgroup,
           taxmin = taxminalogy,
           suborder = taxsuborder,
           subgrp = taxsubgrp,
           taxord = taxorder,
           taxtemp = taxtempregime,
           featkind2 = featkind,
           compkind2 = compkind,
           tex2 = texcl2
    ) %>% 
    mutate_if(is.character, replace_na, replace ="(Missing)") %>%
    mutate_if(is.character, str_trim) %>%
    mutate_if(is.factor, fct_explicit_na)
  
  
  # 2.2 Identify wet thin O horizons above mineral soil ####
  # DEFINE wet_thin_O	IF (texture matches "*PEAT*" OR texture matches "*MUCK*" OR texture matches "*MPT*") and hzdept_r==0 and hzdepb_r <= 20
  # and taxord!="histosols" and suborder!="histels" and not isnull(tex2) then 1 else 0.
  # 
  # ASSIGN hzdept_r			IF wet_thin_O==1 then hzdept_r2 else hzdept_r.
  # ASSIGN hzdepb_r 		IF wet_thin_O==1 then hzdepb_r2 else hzdepb_r.
  # ASSIGN tex				IF wet_thin_O==1 then tex2 else tex.
  # ASSIGN texture			IF wet_thin_O==1 then texture2 else texture.
  # ASSIGN claytotal_r		IF wet_thin_O==1 then claytotal_r2 else claytotal_r.
  # ASSIGN sandtotal_r 		IF wet_thin_O==1 then sandtotal_r2 else sandtotal_r.
  # ASSIGN sandvf_r			IF wet_thin_O==1 then sandvf_r2 else sandvf_r.
  # ASSIGN siltco_r			IF wet_thin_O==1 then siltco_r2 else siltco_r.
  # ASSIGN caco3_r			IF wet_thin_O==1 then caco3_r2 else caco3_r.
  # ASSIGN dbthirdbar_r		IF wet_thin_O==1 then dbthirdbar_r2 else dbthirdbar_r.
  
  
  tbl.merge$wet_thin_o <- 
    grepl("(PEAT)|(MUCK)|(MPT)", tbl.merge$texture) &
    tbl.merge$hzdept_r == 0 &
    tbl.merge$hzdepb_r <= 20 &
    tbl.merge$taxord != "histosols" &
    tbl.merge$suborder != "histels" &
    tbl.merge$tex2 != "(Missing)" &
    !is.na(tbl.merge$tex2)
  
  tbl.merge[tbl.merge$wet_thin_o, 
            c("hzdept_r", 
              "hzdepb_r", 
              "tex", 
              "texture", 
              "claytotal_r", 
              "sandtotal_r", 
              "sandvf_r", 
              "siltco_r", 
              "caco3_r", 
              "dbthirdbar_r")] <-
    tbl.merge[tbl.merge$wet_thin_o, 
              c("hzdept_r2", 
                "hzdepb_r2", 
                "tex2", 
                "texture2", 
                "claytotal_r2", 
                "sandtotal_r2", 
                "sandvf_r2", 
                "siltco_r2", 
                "caco3_r2", 
                "dbthirdbar_r2")]
  
  # 2.3 last data gathering steps ####
  # DEFINE rocks       	REGROUP fragvol_r BY hzdpt AGGREGATE SUM. # sum rocks by horizon depth? what is the aggregation level?
  # ASSIGN hzdpt	   	REGROUP hzdpt BY hzdpt AGGREGATE FIRST. # convert hzdpt to unique list? or first hzdpt in a component? 
  # DEFINE rockfrags   	LOOKUP(hzdept_r, hzdpt, rocks).	
  # ASSIGN rockfrags   	IF ISNULL(rockfrags) THEN 0 ELSE rockfrags.
  # ASSIGN siltco_r		IF ISNULL(siltco_r) THEN 0 ELSE siltco_r.
  # ASSIGN sfragcov_r	isnull(sfragcov_r) ? 0 : sfragcov_r.
  # DEFINE caco3cl		isnull(caco3_r) ? 0 : caco3_r.
  
  # first 4 lines of cvir are applied earlier, when defining table 5
  
  tbl.merge <- tbl.merge %>%
    mutate(rockfrags = replace_na(rockfrags, 0)) %>%
    mutate(siltco_r = replace_na(siltco_r, 0)) %>%
    mutate(sfragcov_r = replace_na(sfragcov_r, 0)) %>%
    mutate(caco3cl = replace_na(caco3_r, 0))
  
  # 2.4 assign texture class if needed ####
  
  # # Determines the texture class from total sand, siit, and clay for in-lieu-of textures that have particle size information populated.
  
  # DEFINE tex_calc		IF silttotal_r >= 79.99 and claytotal_r < 11.99 and not isnull(claytotal_r) then "si"
  # ELSE IF silttotal_r >= 49.99 and claytotal_r < 26.99 and (silttotal_r < 79.99 or claytotal_r >= 11.99) and not isnull(silttotal_r) and not isnull(claytotal_r) then "sil"
  # ELSE IF claytotal_r >= 26.99 and claytotal_r < 39.99 and sandtotal_r <= 20.01 and not isnull(claytotal_r) and not isnull(sandtotal_r) then "sicl"
  # ELSE IF claytotal_r >= 39.99 and silttotal_r >= 39.99 and not isnull(silttotal_r) and not isnull(claytotal_r) then "sic"
  # ELSE IF claytotal_r >= 39.99 and sandtotal_r <= 45.01 and silttotal_r < 39.99 and not isnull(claytotal_r) then "c"
  # ELSE IF claytotal_r >= 26.99 and claytotal_r < 39.99 and sandtotal_r > 20.01 and sandtotal_r <= 45.01 and not isnull(silttotal_r) and not isnull(sandtotal_r) and not isnull(claytotal_r) then "cl"
  # ELSE IF claytotal_r >= 6.99 and claytotal_r < 26.99 and silttotal_r >= 27.99 and silttotal_r < 49.99 and sandtotal_r <= 52.01 and not isnull(silttotal_r) and not isnull(sandtotal_r) and not isnull(claytotal_r) then "l"
  # ELSE IF claytotal_r >= 19.99 and claytotal_r < 34.99 and silttotal_r < 27.99 and sandtotal_r > 45.01 and not isnull(silttotal_r) and not isnull(sandtotal_r) and not isnull(claytotal_r) then "scl"
  # ELSE IF claytotal_r >= 34.99 and sandtotal_r > 45.01 and not isnull(sandtotal_r) and not isnull(claytotal_r) then "sc"
  # ELSE IF sandtotal_r > 89.9 then "s"
  # ELSE IF (silttotal_r + 1.5 * claytotal_r) >= 14.99 and (silttotal_r + 2 * claytotal_r) < 29.99 and not isnull(silttotal_r) and not isnull(claytotal_r) then "ls"
  # ELSE IF (sandvf_r >= 30 and sandvc_r + sandco_r + sandmed_r < 15) or (sandfine_r + sandvf_r > 40 and sandvf_r > (sandfine_r + sandvf_r)/2 and sandvc_r + sandco_r + sandmed_r < 15) then "vfsl"
  # ELSE IF not isnull(sandtotal_r) and not isnull(claytotal_r) then "sl"
  # ELSE null.
  # ASSIGN tex		IF isnull(tex) then tex_calc else tex.
  
  
  # calculate silt/sand/soil in cases where 2 of 3 are present
  
  fixclay <- is.na(tbl.merge$claytotal_r) & !is.na(tbl.merge$sandtotal_r) & !is.na(tbl.merge$silttotal_r)
  fixsand <- !is.na(tbl.merge$claytotal_r) & is.na(tbl.merge$sandtotal_r) & !is.na(tbl.merge$silttotal_r)
  fixsilt <- !is.na(tbl.merge$claytotal_r) & !is.na(tbl.merge$sandtotal_r) & is.na(tbl.merge$silttotal_r)
  
  tbl.merge[fixclay, "claytotal_r"] <- 100 - tbl.merge[fixclay, "sandtotal_r"] - tbl.merge[fixclay, "silttotal_r"]
  tbl.merge[fixsand, "sandtotal_r"] <- 100 - tbl.merge[fixsand, "claytotal_r"] - tbl.merge[fixsand, "silttotal_r"]
  tbl.merge[fixsilt, "silttotal_r"] <- 100 - tbl.merge[fixsilt, "sandtotal_r"] - tbl.merge[fixsilt, "claytotal_r"]
  
  rm(fixclay, fixsand, fixsilt)
  
  # convert texture codes to abbreviations to match nasis code, using str_replace_all set
  vec.textureCodes <- c(
    "^Clay$" = "c",
    "^Clay loam$" = "cl",
    "^Coarse sand$" = "cos",
    "^Coarse sandy loam$" = "cosl",
    "^Fine sand$" = "fs",
    "^Fine sandy loam$" = "fsl",
    "^Loam$" = "l",
    "^Loamy coarse sand$" = "lcos",
    "^Loamy fine sand$" = "lfs", 
    "^Loamy sand$" = "ls",
    "^Loamy very fine sand$" = "lvfs",
    "^Sand$" = "s",
    "^Sandy clay loam$" = "scl",
    "^Sandy loam$" = "sl",
    "^Silt$" = "si",
    "^Silt loam$" = "sil", 
    "^Silty clay$" = "sic", 
    "^Silty clay loam$" = "sicl",
    "^Very fine sand$" = "vfs",
    "^Very fine sandy loam$" = "vfsl"
  )
  tbl.merge$texold <- tbl.merge$tex # the old texture codes are saved, for error checking
  
  tbl.merge$tex <- str_replace_all(tbl.merge$tex, vec.textureCodes)
  
  # define texture calculation ##
  ### note 8/20
  ### error in here from nasis, will need to fix later -- use aqp::ssc_to_texcl() OFF GITHUB  21`76`
  
  fn.tex_calc <- function(silt, sand, clay, vfsand, vcsand, cosand, medsand, fsand) {
    if(is.na(silt) | is.na(clay)) {
      return(NA)
    } else if (silt >= 79.99 & clay < 11.99){
      return("si") 
    } else if (silt >= 49.99 & clay < 26.99 & (silt < 79.99 | clay >= 11.99)) {
      return("sil")
    } else if(is.na(sand)){
      return(NA)
    } else if (clay >= 26.99 & clay < 39.99 & sand <= 20.01) {
      return("sicl")
    } else if  (clay >= 39.99 & silt >= 39.99) {
      return("sic")
    } else if  (clay >= 39.99 & sand <= 45.01 & silt < 39.99) {
      return("c")
    } else if  (clay >= 26.99 & clay < 39.99 & sand > 20.01 & sand <= 45.01) {
      return("cl")
    } else if  (clay >= 6.99 & clay < 26.99 & silt >= 27.99 & silt < 49.99 & sand <= 52.01) {
      return("l")
    } else if  (clay >= 19.99 & clay < 34.99 & silt < 27.99 & sand > 45.01) {
      return("scl")
    } else if  (clay >= 34.99 & sand > 45.01) {
      return("sc")
    } else if  (sand > 89.9) {
      return("s")
    } else if  ((silt + 1.5 * clay) >= 14.99 & (silt + 2 * clay) < 29.99) {
      return("ls")
    } else if(any(is.na(vfsand), is.na(vcsand), is.na(cosand), is.na(medsand), is.na(fsand))) {
      return("sl")
    } else if  ((vfsand >= 30 & vcsand + cosand + medsand < 15) | 
                (fsand + vfsand > 40 & vfsand > (fsand + vfsand)/2 & vcsand + cosand + medsand < 15)) {
      return("vfsl")
    } else if  (!is.na(sand) & !is.na(clay)) {
      return("sl")
    } else return(NA)
  }
  
  ###  OPTIMIZATION POINT HERE. DONT NEED TO MAPPLY OVER THE FULL DATASET
  tbl.merge$texcalc <- 
    mapply(fn.tex_calc, 
           sand = tbl.merge$sandtotal_r,
           clay = tbl.merge$claytotal_r,
           silt = tbl.merge$silttotal_r,
           vfsand = tbl.merge$sandvf_r,
           vcsand = tbl.merge$sandvc_r,
           cosand = tbl.merge$sandco_r,
           medsand = tbl.merge$sandmed_r,
           fsand = tbl.merge$sandfine_r)
  
  
  ### note 8/20
  ### try forcing this on all the rows
  
  ## change point 821
  tbl.merge[is.na(tbl.merge$tex) | tbl.merge$tex == "(Missing)","tex"] <- 
    tbl.merge[is.na(tbl.merge$tex) | tbl.merge$tex == "(Missing)","texcalc"]
  
  
  # 2.5 identify unique soil types (wet surface, moist muck, andic, vitric) ####
  # DEFINE wetsurface	IF (ISNULL(taxtemp) AND months >= 63) OR
  # ((taxtemp =="cryic" OR taxtemp =="pergelic") AND months >= 15) OR
  # ((taxtemp =="frigid" OR taxtemp =="mesic" OR taxtemp =="isofrigid") AND months >= 35) OR
  # ((taxtemp =="thermic" OR taxtemp =="hyperthermic") AND months >= 49)  OR
  # ((taxtemp =="isothermic" OR taxtemp =="isohyperthermic" OR taxtemp =="isomesic") AND months >= 63) THEN 1 ELSE 0.
  
  tbl.merge$wetsurface <-
    ((tbl.merge$taxtemp == "(Missing)" | is.na(tbl.merge$taxtemp)) & tbl.merge$months >= 63) |
    ((tbl.merge$taxtemp =="cryic" | tbl.merge$taxtemp =="pergelic") & tbl.merge$months >= 15) |
    ((tbl.merge$taxtemp =="frigid" | tbl.merge$taxtemp =="mesic" | tbl.merge$taxtemp =="isofrigid") & tbl.merge$months >= 35) |
    ((tbl.merge$taxtemp =="thermic" | tbl.merge$taxtemp =="hyperthermic") & tbl.merge$months >= 49)  |
    ((tbl.merge$taxtemp =="isothermic" | tbl.merge$taxtemp =="isohyperthermic" | tbl.merge$taxtemp =="isomesic") & tbl.merge$months >= 63)
  
  tbl.merge[is.na(tbl.merge$wetsurface), "wetsurface"] <- FALSE
  
  # DEFINE moistmuck	IF (texture matches "*PEAT*" OR texture matches "*MUCK*" OR texture matches "*MPT*") AND locase(localphase) matches "*undrained*" THEN 1 
  # ELSE 0.
  
  tbl.merge$moistmuck <-  grepl("(PEAT)|(MUCK)|(MPT)", tbl.merge$texture) &
    grepl("undrained", tolower(tbl.merge$localphase))
  
  # DEFINE andic	IF ((texture matches "*ASHY*" AND featkind2 =="andic soil properties" AND NOT ISNULL(featkind2) AND 
  #                   (featdept_r ==0 OR featdept_r == hzdept_r)) OR 
  #                  ((texture matches "*MEDL*" OR texture matches "*HYDR*") AND sandtotal_r + siltco_r >= 30 AND dbthirdbar_r > 0.9)) OR
  # (((NOT ISNULL(featkind2) AND featkind2 =="andic soil properties" AND (featdept_r ==0 OR featdept_r == hzdept_r)) OR 
  #   texture matches "*MEDL*" OR texture matches "*HYDR*")) THEN 1 ELSE 0.
  # 
  
  tbl.merge$andic <-  # no andic soils in UT, need to test with another database eg OR
    (
      (
        grepl("ASHY", tbl.merge$texture) & 
          tolower(tbl.merge$featkind2) == "andic soil properties" &
          !is.na(tbl.merge$featkind2) & 
          tbl.merge$featkind2 != "(Missing)" &
          (
            tbl.merge$featdept_r == 0 | 
              tbl.merge$featdept_r == tbl.merge$hzdept_r
          )
      ) | (
        grepl("(MEDL)|(HYDR)", tbl.merge$texture) &
          tbl.merge$sandtotal_r + tbl.merge$siltco_r >= 30 &
          tbl.merge$dbthirdbar_r > 0.9
      ) 
    ) | (
      (
        tolower(tbl.merge$featkind2) == "andic soil properties" &
          !is.na(tbl.merge$featkind2) &
          tbl.merge$featkind2 != "(Missing)" &
          (
            tbl.merge$featdept_r == 0 |
              tbl.merge$featdept_r == tbl.merge$hzdept_r
          )
      ) |
        grepl("(MEDL)|(HYDR)", tbl.merge$texture)
    )   
  
  tbl.merge[is.na(tbl.merge$andic), "andic"] <- FALSE
  
  # DEFINE vitri	IF andic == 0 AND texture matches "*ASHY*" AND ((subgrp matches "vitrandic*" OR 
  #                                                               subgrp matches "vitritorrandic*" OR subgrp matches "vitrixerandic*" OR subgrp matches "ustivitrandic*") OR
  #                                                              (taxord != "andisols" AND (featkind2 != "andic soil properties" OR ISNULL(featkind)))) THEN 1 ELSE 0.
  # 
  
    tbl.merge$vitri <-
      !tbl.merge$andic &
      grepl("ASHY", tbl.merge$texture) & 
      (
        grepl("(vitrand)|(vitritorrand)|(vitrixerand)|(ustivitrand)", tolower(tbl.merge$subgrp)) | 
          (
            tolower(tbl.merge$taxord)  != "andisols" & 
              (
                tolower(tbl.merge$featkind2) != "andic soil properties" |
                  is.na(tbl.merge$featkind2) |
                  tbl.merge$featkind2 != "(Missing)"
              )
          )
      )



  
  
  tbl.merge[is.na(tbl.merge$vitri),"vitri"] <- F
  
  # DEFINE rfrags	IF texture=="ART" or texture=="BY" or texture=="CB" or texture=="CN" or texture=="FL" or texture=="GR" or texture=="ST"
  # or texture=="PBY" or texture=="PCB" or texture=="PCN" or texture=="PFL" or texture=="PG" or texture=="PST" THEN 1 ELSE 0.
  
  tbl.merge$rfrags <-
    tbl.merge$texture %in% c("ART", "BY", "CB", "CN", "FL", "GR", "ST", "PBY", "PCB", "PCN", "PFL", "PG", "PST")
  
  #### temporary: add on the wei and weg calcs for each component, for checking
  tbl.component$cokey <- as.character(tbl.component$cokey)
  
  
  tbl.merge <- 
    left_join(tbl.merge, 
              tbl.component %>%
                dplyr::select(
                  cokey,
                  weg,
                  wei
                ), by = "cokey") %>%
    mutate_if(is.character, replace_na, replace ="(Missing)") %>%
    mutate_if(is.factor, fct_explicit_na) %>%
    mutate_if(is.character, str_trim)
  
  
  
  if(intermediates){
    out <- c(list(tbl.merge), ls.SummaryTables)
    names(out) <- c("FinalAggregated", "intermediate1", "intermediate2", "intermediate3", "intermediate4", 
                    "intermediate5", "intermediate6", "intermediate7")
  } else {
    out <- tbl.merge
  }
  
  return(out)
  
}
