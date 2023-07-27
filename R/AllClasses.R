#' An S4 class to contain all input data as well as a dictionary to link them.
#'
#' @slot metadata A matrix of feature metadata. Rows are samples and columns are metadata information.
#' @slot tableX A matrix of feature counts. Rows are samples and columns are features.
#' @slot dictionary A asymmetric, square edge matrix (i rows by j columns) that represents causal effects of feature i on feature j
#' @description CauDAbox is the main container that will hold your input data thoughout the \code{CauDA} pipeline.
#'
setClass("CauDAbox",
         slots = c(
           metadata   = "matrix",
           tableX     = "matrix",
           dictionary = "matrix"
         )
)
