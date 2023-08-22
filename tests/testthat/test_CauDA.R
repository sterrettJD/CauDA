make_simple_confounded_dataset <- function(nsamples=1000,
                                           c_on_x=0.5,
                                           x_on_y=0.5,
                                           c_on_y=0.4){
  set.seed(42)
  # confounder
  tax.c <- rnbinom(n=nsamples, size=500, prob=0.3)
  # taxon X (independent var)
  tax.x <- c_on_x*tax.c + (1-c_on_x)*rnbinom(n=nsamples, size=500, prob=0.3)
  # Y (dependent var)
  y <- x_on_y*tax.x +
       c_on_y*tax.c +
       (1-x_on_y-c_on_y)*rnorm(nsamples, mean=mean(tax.c), sd=sd(tax.c))
  # edge matrix to show that tax.c has a causal effect on tax.x
  edge.dictionary <- matrix(c(0, 0,
                              1, 0),
                            nrow=2,
                            byrow=T)
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

testthat::test_that("confounded taxa have correct coef", {
  c.on.x <- 0.5
  x.on.y <- 0.5
  c.on.y <- 0.4
  data <- make_simple_confounded_dataset(1000,
                                         c_on_x=c.on.x,
                                         x_on_y=x.on.y,
                                         c_on_y=c.on.y)

  y <- data@metadata[,1]
  tax.x <- data@tableX[,1]
  tax.c <- data@tableX[,2]
  mod <- lm(y ~ tax.x + tax.c)

  expect_equal(coef(mod)[2:3],
               c(x.on.y, c.on.y),
               tolerance=0.05,
               ignore_attr = TRUE)
}
)

testthat::test_that("confounded taxon is confounded", {
  c.on.x <- 0.5
  x.on.y <- 0.5
  c.on.y <- 0.4
  data <- make_simple_confounded_dataset(1000,
                                         c_on_x=c.on.x,
                                         x_on_y=x.on.y,
                                         c_on_y=c.on.y)

  y <- data@metadata[,1]
  tax.x <- data@tableX[,1]
  tax.c <- data@tableX[,2]
  mod <- lm(y ~ tax.x) # LEAVE OUT THE CONFOUNDER

  expect_failure(
    expect_equal(coef(mod)[2],
               x.on.y,
               tolerance=0.05,
               ignore_attr = TRUE))
}
)



