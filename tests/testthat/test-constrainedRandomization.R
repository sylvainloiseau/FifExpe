library(testthat)
test_that("permute.without.repetition1", {
  data <- data.frame(A=c(rep("a", 3), rep("b", 3)), B=c(rep("c", 3), rep("d", 3)));
  x <- order.without.repetition(data, c("A", "B"), c(1,1));
  expect_true(all(table(x$A) == 3))
  expect_true(all(table(x$B) == 3))
})

test_that("permute.without.repetition2", {
  data <- data.frame(A=LETTERS[1:10], B=rep("un", 10), C=1:10)
  expect_error(
    order.without.repetition(data, c("A", "B"), c(1,1)),
    "the maximum number of tries has been reached"
  );
})

