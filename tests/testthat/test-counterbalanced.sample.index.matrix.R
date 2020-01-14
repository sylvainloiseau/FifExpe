context("balancedSampling")

test_that("fill.counterbalanced.sample.index.matrix", {
  data <- data.frame(
    A=rep(LETTERS[1:4], times=2),
    B=rep(letters[1:2], each=4)
  )
  maxtry <- 50;
  success <- FALSE;
  ntry <- 0;
  while(!success) {
    res <- fill.counterbalanced.sample.index.matrix(data, c("A", "B"), c(4, 2), c(2, 4), 8, 2, 4);
    ntry <- ntry + 1
    if (ntry == maxtry) {
      break;
    }
    success <- res[1]
  }
})

test_that("balancedSamping", {
  data <- data.frame(
    A=rep(LETTERS[1:4], times=2),
    B=rep(letters[1:2], each=4)
  )

  res <- balancedSampling(data, vars=c("A", "B"), sample.nb=2)

  sample1.A <- table(res[[1]][,"A"])
  expect_true(all(sample1.A == 1))

  sample1.B <- table(res[[1]][,"B"])
  expect_true(all(sample1.B == 2))

  sample2.A <- table(res[[2]][,"A"])
  expect_true(all(sample2.A == 1))

  sample2.B <- table(res[[2]][,"B"])
  expect_true(all(sample2.B == 2))
})

test_that("counterbalanced.sample.index.matrix: wrong arguments 1", {
  expect_error(counterbalanced.sample.index.matrix(1, 3, 4));
})

test_that("counterbalanced.sample.index.matrix: wrong arguments 2", {
  expect_error(counterbalanced.sample.index.matrix(1, 1, 4));
})

test_that("counterbalanced.sample.index.matrix: wrong arguments 3", {
  expect_error(counterbalanced.sample.index.matrix(5, 5, 4));
})

