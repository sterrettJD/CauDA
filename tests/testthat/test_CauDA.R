make_simple_confounded_dataset <- function(nsamples=1000,
                                           c_on_x=0.5,
                                           x_on_y=0.5,
                                           c_on_y=0.4){
  set.seed(42)
  # confounder
  tax.c <- rnbinom(n=nsamples, size=500, prob=0.3)
  # taxon X (independent var)
  tax.x <- c_on_x*tax.c + (1-c_on_x)*rnbinom(n=nsamples, size=500, prob=0.3)
  # matrix of taxa
  count.table <- matrix(data=c(tax.x, tax.c), nrow=nsamples)
  colnames(count.table) <- c("tax.x", "tax.c")

  # Y (dependent var)
  y <- x_on_y*tax.x +
       c_on_y*tax.c +
       (1-x_on_y-c_on_y)*rnorm(nsamples, mean=mean(tax.c), sd=sd(tax.c))

  # edge matrix to show that tax.c has a causal effect on tax.x
  edge.dictionary <- matrix(c(0,         0,
                              c_on_x!=0, 0),
                            nrow=2,
                            byrow=T)
  rownames(edge.dictionary) <- c("tax.x", "tax.c")
  colnames(edge.dictionary) <- c("tax.x", "tax.c")

  new("CauDAbox",
      metadata=matrix(data=y, nrow=nsamples),
      tableX=count.table,
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

# set some params
c.on.x <- 0.5
x.on.y <- 0.5
c.on.y <- 0.4
# make a confounded dataset for testing
main.confounded.data <- make_simple_confounded_dataset(1000,
                                       c_on_x=c.on.x,
                                       x_on_y=x.on.y,
                                       c_on_y=c.on.y)

# test that the class works
testthat::test_that("create dataset", {
  x <- make_simple_confounded_dataset(500)
  expect_s4_class(x, "CauDAbox")
  expect_s4_class(main.confounded.data, "CauDAbox")
}
)

# test that properly regressing confounded data
# returns correct coefficients
testthat::test_that("confounded taxa have correct coef", {
  y <- main.confounded.data@metadata[,1]
  tax.x <- main.confounded.data@tableX[,1]
  tax.c <- main.confounded.data@tableX[,2]
  mod <- lm(y ~ tax.x + tax.c)

  expect_equal(coef(mod)[2:3],
               c(x.on.y, c.on.y),
               tolerance=0.05,
               ignore_attr = TRUE)
}
)

# test that improperly regressing confounded data
# returns the wrong coefficients
testthat::test_that("confounded taxon is confounded", {
  y <- main.confounded.data@metadata[,1]
  tax.x <- main.confounded.data@tableX[,1]
  tax.c <- main.confounded.data@tableX[,2]
  mod <- lm(y ~ tax.x) # LEAVE OUT THE CONFOUNDER

  expect_failure(
    expect_equal(coef(mod)[2],
               x.on.y,
               tolerance=0.05,
               ignore_attr = TRUE))
}
)

# Test that extracting the formula from a CauDAbox
# returns the right formula
testthat::test_that("RHS_from_edgemat gets the right stuff", {
  expect_equal(RHS_from_edgemat(main.confounded.data,
                                "tax.x"),
               "tax.x + tax.c")
}
)

# Test that regressing on the returned formula
# returns the correct coefficients
testthat::test_that("RHS_from_edgemat gets the right model output", {
  rhs <- RHS_from_edgemat(main.confounded.data, "tax.x")
  y <- main.confounded.data@metadata[,1]
  mod <- lm(as.formula(paste0("y ~ ", rhs)),
            data=make_model_df(main.confounded.data, "tax.x"))

  expect_equal(coef(mod)[2:3],
               c(x.on.y, c.on.y),
               tolerance=0.05,
               ignore_attr = TRUE)
}
)

