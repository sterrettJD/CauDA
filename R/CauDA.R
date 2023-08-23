get_cols_to_include <- function(caudabox, column){
  dict <- caudabox@dictionary

  taxon.info <- dict[,column]
  to.include <- c(column, names(which(taxon.info==1)))
  return(to.include)
}

RHS_from_edgemat <- function(caudabox, column){

  to.include <- get_cols_to_include(caudabox, column)
  RHS <- paste(to.include, collapse=" + ")
  return(RHS)
}

make_model_df <- function(caudabox, column){
  y <- caudabox@metadata
  x <- caudabox@tableX
  to.include <- get_cols_to_include(caudabox, column)

  data <- cbind(y, x[,to.include])
  data <- as.data.frame(data)
  colnames(data) <- c("y", colnames(x[,to.include]))
  return(data)
}
