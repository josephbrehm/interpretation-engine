require(data.tree)

### 1.1 define hsg tree #### 
# All trees are defined in the same way:
## create the empy tree
## add nodes one at a time -- three types, root, decision, and classification

## root node isn't used anywhere else. They get the default name "site" and no other attributes

## all decision nodes need three values: 
### name (not essential for the decision nodes, used for labelling)
### logical (essential, the logical statement evaluated by the decision)
### var (essential, the exact variable name evaluated. Same name must be used exactly in 'logical')

## all classification nodes need only a name, this is the value returned by the decision function 
### Here they also get "classification" for the var entry, which is a relic


### root
tr.hsg <- Node$new(name = "site")

### level 1 restrictive layer evaluation ###
tr.hsg$AddChild("Shallow restrictive layer",
                logical = "rl < 50 & !is.na(rl)", #have to add in the not na portion whenever an na value is allowed elsewhere on the same tier
                var = "rl")
tr.hsg$AddChild("Moderate depth restrictive layer",
                logical = "rl >= 50 & rl < 100 & !is.na(rl)", 
                var = "rl")
tr.hsg$AddChild("Deep restrictive layer",
                logical = "rl >= 100 | is.na(rl)",
                var = "rl")

### level 2 water table depth evaluation ###
## shallow rl  
tr.hsg$`Shallow restrictive layer`$AddChild("D",
                                            var = "classification")
## (this ends the shallow rl branch)

## moderate rl
tr.hsg$`Moderate depth restrictive layer`$AddChild("Shallow water table",
                                                   logical = "wt < 60",
                                                   var = "wt")
tr.hsg$`Moderate depth restrictive layer`$AddChild("Deep water table",
                                                   logical = "wt >= 60",
                                                   var = "wt")

## deep rl
tr.hsg$`Deep restrictive layer`$AddChild("Shallow water table",
                                         logical = "wt < 60",
                                         var = "wt")
tr.hsg$`Deep restrictive layer`$AddChild("Moderate depth water table",
                                         logical = "wt >= 60 & wt < 100",
                                         var = "wt")
tr.hsg$`Deep restrictive layer`$AddChild("Deep water table",
                                         logical = "wt >= 100",
                                         var = "wt")

### level 3a ksat, moderate RL, shallow WT ###
tr.hsg$`Moderate depth restrictive layer`$`Shallow water table`$AddChild("High conductivity",
                                                                         logical = "ksat > 10",
                                                                         var = "ksat")

tr.hsg$`Moderate depth restrictive layer`$`Shallow water table`$AddChild("Moderate-high conductivity",
                                                                         logical = "ksat <= 10 & ksat > 4",
                                                                         var = "ksat")

tr.hsg$`Moderate depth restrictive layer`$`Shallow water table`$AddChild("Moderate-low conductivity",
                                                                         logical = "ksat <= 4 & ksat > 0.4",
                                                                         var = "ksat")

tr.hsg$`Moderate depth restrictive layer`$`Shallow water table`$AddChild("Low conductivity",
                                                                         logical = "ksat <= 0.4",
                                                                         var = "ksat")

## classification nodes (this could be rewritten to store this data in the parent nodes, as they don't branch any further by definition)
## but that would make display harder, this method results in all classes being printed
tr.hsg$`Moderate depth restrictive layer`$`Shallow water table`$`High conductivity`$AddChild(
  "A/D", 
  var = "classification")

tr.hsg$`Moderate depth restrictive layer`$`Shallow water table`$`Moderate-high conductivity`$AddChild(
  "B/D", 
  var = "classification")

tr.hsg$`Moderate depth restrictive layer`$`Shallow water table`$`Moderate-low conductivity`$AddChild(
  "C/D", 
  var = "classification")

tr.hsg$`Moderate depth restrictive layer`$`Shallow water table`$`Low conductivity`$AddChild(
  "D", 
  var = "classification")

### level 3b ksat, moderate rl, deep wt ###
##
tr.hsg$`Moderate depth restrictive layer`$`Deep water table`$AddChild("High conductivity",
                                                                      logical = "ksat > 40",
                                                                      var = "ksat")
tr.hsg$`Moderate depth restrictive layer`$`Deep water table`$AddChild("Moderate-high conductivity",
                                                                      logical = "ksat <= 40 & ksat > 10",
                                                                      var = "ksat")
tr.hsg$`Moderate depth restrictive layer`$`Deep water table`$AddChild("Moderate-low conductivity",
                                                                      logical = "ksat <= 10 & ksat > 1",
                                                                      var = "ksat")
tr.hsg$`Moderate depth restrictive layer`$`Deep water table`$AddChild("Low conductivity",
                                                                      logical = "ksat <= 1",
                                                                      var = "ksat")
##
tr.hsg$`Moderate depth restrictive layer`$`Deep water table`$`High conductivity`$AddChild(
  "A", 
  var = "classification")

tr.hsg$`Moderate depth restrictive layer`$`Deep water table`$`Moderate-high conductivity`$AddChild(
  "B", 
  var = "classification")

tr.hsg$`Moderate depth restrictive layer`$`Deep water table`$`Moderate-low conductivity`$AddChild(
  "C", 
  var = "classification")

tr.hsg$`Moderate depth restrictive layer`$`Deep water table`$`Low conductivity`$AddChild(
  "D", 
  var = "classification")

### level 3c ksat, deep rl, shallow wt###
tr.hsg$`Deep restrictive layer`$`Shallow water table`$AddChild("High conductivity",
                                                               logical = "ksat > 10",
                                                               var = "ksat")
tr.hsg$`Deep restrictive layer`$`Shallow water table`$AddChild("Moderate-high conductivity",
                                                               logical = "ksat <= 10 & ksat > 4",
                                                               var = "ksat")
tr.hsg$`Deep restrictive layer`$`Shallow water table`$AddChild("Moderate-low conductivity",
                                                               logical = "ksat <= 4 & ksat > 0.4",
                                                               var = "ksat")
tr.hsg$`Deep restrictive layer`$`Shallow water table`$AddChild("Low conductivity",
                                                               logical = "ksat <= 0.4",
                                                               var = "ksat")
tr.hsg$`Deep restrictive layer`$`Shallow water table`$`High conductivity`$AddChild(
  "A/D", 
  var = "classification")

tr.hsg$`Deep restrictive layer`$`Shallow water table`$`Moderate-high conductivity`$AddChild(
  "B/D", 
  var = "classification")

tr.hsg$`Deep restrictive layer`$`Shallow water table`$`Moderate-low conductivity`$AddChild(
  "C/D", 
  var = "classification")

tr.hsg$`Deep restrictive layer`$`Shallow water table`$`Low conductivity`$AddChild(
  "D", 
  var = "classification")

### level 3d ksat, deep rl, moderate wt###

tr.hsg$`Deep restrictive layer`$`Moderate depth water table`$AddChild("High conductivity",
                                                                      logical = "ksat > 40",
                                                                      var = "ksat")
tr.hsg$`Deep restrictive layer`$`Moderate depth water table`$AddChild("Moderate-high conductivity",
                                                                      logical = "ksat <= 40 & ksat > 10",
                                                                      var = "ksat")
tr.hsg$`Deep restrictive layer`$`Moderate depth water table`$AddChild("Moderate-low conductivity",
                                                                      logical = "ksat <= 10 & ksat > 1",
                                                                      var = "ksat")
tr.hsg$`Deep restrictive layer`$`Moderate depth water table`$AddChild("Low conductivity",
                                                                      logical = "ksat <= 1",
                                                                      var = "ksat")

tr.hsg$`Deep restrictive layer`$`Moderate depth water table`$`High conductivity`$AddChild(
  "A", 
  var = "classification")

tr.hsg$`Deep restrictive layer`$`Moderate depth water table`$`Moderate-high conductivity`$AddChild(
  "B", 
  var = "classification")

tr.hsg$`Deep restrictive layer`$`Moderate depth water table`$`Moderate-low conductivity`$AddChild(
  "C", 
  var = "classification")

tr.hsg$`Deep restrictive layer`$`Moderate depth water table`$`Low conductivity`$AddChild(
  "D", 
  var = "classification")


### level 3d ksat, deep rl, deep wt###

tr.hsg$`Deep restrictive layer`$`Deep water table`$AddChild("High conductivity",
                                                            logical = "ksat > 10",
                                                            var = "ksat")
tr.hsg$`Deep restrictive layer`$`Deep water table`$AddChild("Moderate-high conductivity",
                                                            logical = "ksat <= 10 & ksat > 4",
                                                            var = "ksat")
tr.hsg$`Deep restrictive layer`$`Deep water table`$AddChild("Moderate-low conductivity",
                                                            logical = "ksat <= 4 & ksat > .4",
                                                            var = "ksat")
tr.hsg$`Deep restrictive layer`$`Deep water table`$AddChild("Low conductivity",
                                                            logical = "ksat <= .4",
                                                            var = "ksat")

##
tr.hsg$`Deep restrictive layer`$`Deep water table`$`High conductivity`$AddChild(
  "A", 
  var = "classification")

tr.hsg$`Deep restrictive layer`$`Deep water table`$`Moderate-high conductivity`$AddChild(
  "B", 
  var = "classification")

tr.hsg$`Deep restrictive layer`$`Deep water table`$`Moderate-low conductivity`$AddChild(
  "C", 
  var = "classification")

tr.hsg$`Deep restrictive layer`$`Deep water table`$`Low conductivity`$AddChild(
  "D", 
  var = "classification")

tr.hsg

save(tr.hsg, file = "datatree-hsg.rdata")

