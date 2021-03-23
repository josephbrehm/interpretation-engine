require(caret)

cm_refactor <- function(predvar, refvar){
  # refactor both inputs to have the same levels as one another
  
  refvar <- fct_explicit_na(as.character(refvar))
  predvar <- fct_explicit_na(as.character(predvar))
  
  predvar <- factor(x = predvar, 
                    levels = levels(refvar))
  refvar <- factor(x = refvar,
                   levels = levels(predvar))
  
  cm <- confusionMatrix(reference = refvar,
                        data = predvar)
  return(cm)
}
