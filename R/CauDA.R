caudabox_from_csvs <- function(metadata, tableX, dictionary){
  paths <- c(metadata, tableX, dictionary)
  files.exist <- file.exists(paths)
  if(sum(files.exist) < 3){
    stop(paste("Not all files provided exist. Please check",
               paths[which(!files.exist)]))
  }

  m <- read.csv(metadata)
  X <- read.csv(tableX)
  d <- read.csv(dictionary)
  d <- as.matrix(d, by.row=F, nrow=nrow(d))
  row.names(d) <- colnames(d)

  return(new("CauDAbox",
             metadata=as.matrix(m, by.row=F, nrow=nrow(m)),
             tableX=as.matrix(X, by.row=F, nrow=nrow(X)),
             dictionary=d))
}

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
