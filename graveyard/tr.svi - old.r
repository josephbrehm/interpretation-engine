require(data.tree)

### 1.2 define svi tree ####
tr.svi <- Node$new(name = "site")

### level 1 hsg ###
tr.svi$AddChild("HSG A",
                logical = "shorthsg == 'A'",
                var = "shorthsg") 
# a Shorthsg is an abberation found in the underdark. Beware its razor sharp talons.
# (have to use a shortened version of hsg, swapping eg A/D for A)

tr.svi$AddChild("HSG B",
                logical = "shorthsg == 'B'",
                var = "shorthsg")
tr.svi$AddChild("HSG C",
                logical = "shorthsg == 'C'",
                var = "shorthsg")
tr.svi$AddChild("HSG D",
                logical = "shorthsg == 'D'",
                var = "shorthsg")

### level 2 slope ###
## A
tr.svi$`HSG A`$AddChild("1 - Low",
                        var = "classification")

## B
tr.svi$`HSG B`$AddChild("Low slope",
                        logical = "slope < 4",
                        var = "slope")
tr.svi$`HSG B`$AddChild("Moderate slope",
                        logical = "slope >= 4 & slope <= 6",
                        var = "slope")
tr.svi$`HSG B`$AddChild("High slope",
                        logical = "slope > 6",
                        var = "slope")

## C
tr.svi$`HSG C`$AddChild("Low slope",
                        logical = "slope < 2",
                        var = "slope")
tr.svi$`HSG C`$AddChild("Moderate slope",
                        logical = "slope >= 2 & slope <= 6",
                        var = "slope")
tr.svi$`HSG C`$AddChild("High slope",
                        logical = "slope > 6",
                        var = "slope")


## D
tr.svi$`HSG D`$AddChild("Low slope",
                        logical = "slope < 2",
                        var = "slope")
tr.svi$`HSG D`$AddChild("Moderate slope",
                        logical = "slope >= 2 & slope <= 4",
                        var = "slope")
tr.svi$`HSG D`$AddChild("High slope",
                        logical = "slope > 4",
                        var = "slope")

### level 3 kw ###
tr.svi$`HSG B`$`Moderate slope`$AddChild("Low kw",
                                         logical = "kw < 0.32",
                                         var = "kw")

tr.svi$`HSG B`$`Moderate slope`$AddChild("High kw",
                                         logical = "kw >= 0.32",
                                         var = "kw")


##
tr.svi$`HSG C`$`Moderate slope`$AddChild("Low kw",
                                         logical = "kw < 0.28",
                                         var = "kw")
tr.svi$`HSG C`$`Moderate slope`$AddChild("High kw",
                                         logical = "kw >= 0.28",
                                         var = "kw")

##
tr.svi$`HSG D`$`Low slope`$AddChild("Low kw",
                                    logical = "kw < 0.28",
                                    var = "kw")
tr.svi$`HSG D`$`Low slope`$AddChild("High kw",
                                    logical = "kw >= 0.28",
                                    var = "kw")

### classifications (on several levels)
tr.svi$`HSG B`$`Low slope`$AddChild("1 - Low",
                                    var = "classification")

tr.svi$`HSG B`$`Moderate slope`$`Low kw`$AddChild("2 - Moderate",
                                                  var = "classification")

tr.svi$`HSG B`$`Moderate slope`$`High kw`$AddChild("3 - Moderately High",
                                                   var = "classification")

tr.svi$`HSG B`$`High slope`$AddChild("4 - High",
                                     var = "classification")

##
tr.svi$`HSG C`$`Low slope`$AddChild("1 - Low",
                                    var = "classification")

tr.svi$`HSG C`$`Moderate slope`$`Low kw`$AddChild("2 - Moderate",
                                                  var = "classification")

tr.svi$`HSG C`$`Moderate slope`$`High kw`$AddChild("3 - Moderately High",
                                                   var = "classification")

tr.svi$`HSG C`$`High slope`$AddChild("4 - High",
                                     var = "classification")

##

tr.svi$`HSG D`$`Low slope`$`Low kw`$AddChild("1 - Low",
                                             var = "classification")

tr.svi$`HSG D`$`Low slope`$`High kw`$AddChild("2 - Moderate",
                                              var = "classification")

tr.svi$`HSG D`$`Moderate slope`$AddChild("3 - Moderately High",
                                         var = "classification")

tr.svi$`HSG D`$`High slope`$AddChild("4 - High",
                                     var = "classification")
####

save(tr.svi, file = "datatree-svi.rdata")
