# Decision tree evaluation for data.tree
# Joe Brehm
# Last edited 8/31/2020

# this function evaluates decision trees stored as data.tree type objects
# input requirements are fairly specific: see the associated tree creation files for how to create trees that can work here

require(data.tree)

tree_eval <- function(tree, 
                      # tree is a data tree object structured with classifications as leaves
                      # above those, all nodes must contain these attributes:
                      # "var", specifying the variable name to evaluate,
                      # "logical", specifying the logical evaluation at that step. MUST INCLUDE THE SAME VARIABLE "VAR"
                      data
                      # data must be a data frame containing columns with names matching all the values of "var" used in the tree object
){
  
  parent = tree$root # parent holds the set of nodes to be evaluated; start as the root.
  descend = F # when descend becomes true, the function has found the right path to follow and will look at the next set of siblings down
  i = 1 # each child node of the current parent will be evaluated in turn, referred to by index "i"
  
  node = parent$children[[i]] # node holds the exact logical node to be evaluated, the i'th node of parent
  
  while(!isLeaf(node))  { # when the node is a leaf (end node), the function is done
    
    # extract the name of the variable to evaluate
    varname = node$var
    
    # check to see if its in the input data
    if(!(varname %in% colnames(data))) stop(paste(varname, "must be a column name in input data"))
    
    # extract the value
    value = data[,varname]
    
    # if the value is na, this point can't be evaluated due to missing data
    if(is.na(value) & !grepl("is.na", node$logical)) return(NA)
    
    # change the logical string to use the generic 'value' instead of the specified variable name
    # specific variable names are used in the tree objects for readability
    lstr = gsub(varname, "value", node$logical)
    
    # evaluate the logical string, storing T/F to 'descend'
    descend = eval(parse(text = lstr)) 
    
    # if descend is true, then this is the correct path. Set the current node as the parent, and start over!
    if(descend) {
      parent = node
      i = 1
      
      # if descend is false, check the next sibling node at this level  
    } else {
      i = i + 1
    }
    
    # if there are no more children to evaluate, then there is an error in the logical pathway of the tree. 
    if(i > length(parent$children)) stop("Error: cannot solve this data point. Check for errors in the tree")
    
    # otherwise, get the next node down and start evaluating again
    node = parent$children[[i]]
  }
  
  return(node$name)
}

