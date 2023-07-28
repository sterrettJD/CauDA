make_simple_confounded_dataset <- function(nsamples=1000){
  # confounder
  tax.c <- rnbinom(n=nsamples, size=500, prob=0.3)
  # taxon X (independent var)
  tax.x <- 0.5*tax.c + 0.5*rnbinom(n=nsamples, size=500, prob=0.3)
  # Y (dependent var)
  y <- 0.5*tax.c + 0.5*rnorm(nsamples, mean=mean(tax.c), sd=sd(tax.c))
  # edge matrix to show that tax.c has a causal effect on tax.x
  edge.dictionary <- matrix(c(0, 0,
                              1, 0),
                            nrow=2)
  rownames(edge.dictionary) <- c("tax.x", "tax.c")
  colnames(edge.dictionary) <- c("tax.x", "tax.c")

  new("CauDAbox",
      metadata=matrix(data=y, nrow=nsamples),
      tableX=matrix(data=c(tax.c, tax.x), nrow=nsamples),
      dictionary=edge.dictionary
      )
}


make_simple_dataset <- function(nsamples){
  new("CauDAbox",
      metadata=matrix(data = c(1:nsamples, rep(0, nsamples/2), rep(1, nsamples/2)), nrow=nsamples),
      tableX=matrix(rnorm(100*nsamples), nrow=nsamples),
      dictionary=diag(nsamples)
      )
}


testthat::test_that("create dataset", {
  x <- make_simple_confounded_dataset(500)
  expect_s4_class(x, "CauDAbox")
}
)


