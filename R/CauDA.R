#' Generate a CauDAbox object from CSVs
#' @description Should be used to read in data from csv files to create a CauDAbox.
#' @param metadata This contains sample metadata, where samples are rows.
#' @param tableX This contains the feature data (e.g., feature count table), where samples are rows
#' @param dictionary This is a N x N edge matrix, where N=number of features in tableX, where each entry (row i, col j) indicates if feature i has a causal effect on feature j.
#' @return A CauDAbox object with the metadata, tableX, and dictionary.
#' @export
#' @importFrom methods new
#' @importFrom utils read.csv
#' 
caudabox_from_csvs <- function(metadata, tableX, dictionary){
  paths <- c(metadata, tableX, dictionary)
  files.exist <- file.exists(paths)
  if(sum(files.exist) < 3){
    stop(paste("Not all files provided exist. Please check",
               paths[which(!files.exist)]))
  }
  
  # read data
  m <- read.csv(metadata)
  X <- read.csv(tableX)
  d <- read.csv(dictionary)
  
  if((nrow(d) != ncol(X)) | (ncol(d) != ncol(X))){
    stop("The provided dictionary is not a square edge matrix with the same number of dimensions as tableX.")
  }
  
  # convert dict to be a matrix with colnames the same as rownames
  d <- as.matrix(d, by.row=F, nrow=nrow(d))
  row.names(d) <- colnames(d)

  return(new("CauDAbox",
             metadata=as.matrix(m, by.row=F, nrow=nrow(m)),
             tableX=as.matrix(X, by.row=F, nrow=nrow(X)),
             dictionary=d))
}

#' PLACEHOLDER
#' @description PLACEHOLDER
#' @param caudabox A caudabox object
#' @param column PLACEHOLDER
#' @export
#' 
get_cols_to_include <- function(caudabox, column){
  dict <- caudabox@dictionary

  taxon.info <- dict[,column]
  to.include <- c(column, names(which(taxon.info==1)))
  return(to.include)
}

#' PLACEHOLDER
#' @description PLACEHOLDER
#' @param caudabox A caudabox object
#' @param column PLACEHOLDER
#' @export
#' 
RHS_from_edgemat <- function(caudabox, column){

  to.include <- get_cols_to_include(caudabox, column)
  RHS <- paste(to.include, collapse=" + ")
  return(RHS)
}

#' PLACEHOLDER
#' @description PLACEHOLDER
#' @param caudabox A caudabox object
#' @param column PLACEHOLDER
#' @export
#' 
make_model_df <- function(caudabox, column){
  y <- caudabox@metadata
  x <- caudabox@tableX
  to.include <- get_cols_to_include(caudabox, column)

  data <- cbind(y, x[,to.include])
  data <- as.data.frame(data)
  colnames(data) <- c("y", colnames(x[,to.include]))
  return(data)
}
