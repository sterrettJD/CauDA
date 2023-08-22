RHS_from_edgemat <- function(caudabox, column){
  dict <- caudabox@dictionary

  taxon.info <- dict[,column]
  to.include <- c(column, names(which(taxon.info==1)))

  RHS <- paste(to.include, collapse=" + ")

  return(RHS)
}
