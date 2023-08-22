make_simple_confounded_dataset <- function(nsamples=1000){
  set.seed(42)
  # confounder
  tax.c <- rnbinom(n=nsamples, size=500, prob=0.3)
  # taxon X (independent var)
  tax.x <- 0.5*tax.c + 0.5*rnbinom(n=nsamples, size=500, prob=0.3)
  # Y (dependent var)
  y <- 0.5*tax.x + 0.4*tax.c + 0.1*rnorm(nsamples, mean=mean(tax.c), sd=sd(tax.c))
  # edge matrix to show that tax.c has a causal effect on tax.x
  edge.dictionary <- matrix(c(0, 0,
                              1, 0),
                            nrow=2)
  rownames(edge.dictionary) <- c("tax.x", "tax.c")
  colnames(edge.dictionary) <- c("tax.x", "tax.c")

  new("CauDAbox",
      metadata=matrix(data=y, nrow=nsamples),
      tableX=matrix(data=c(tax.x, tax.c), nrow=nsamples),
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

testthat::test_that("taxa have correct coef", {
  data <- make_simple_confounded_dataset(1000)

  y <- data@metadata[,1]
  tax.x <- data@tableX[,1]
  tax.c <- data@tableX[,2]
  mod <- lm(y ~ tax.x + tax.c)

  expect_equal(coef(mod)[2:3],
               c(0.5, 0.4),
               tolerance=0.05,
               ignore_attr = TRUE)
}
)



