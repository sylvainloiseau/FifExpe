context("common")

test_that("wrong_argument_vars", {
  data <- data.frame(A=LETTERS[1:10], Z=rep("un", 10), C=1:10)
  expect_error(
    balancedSampling(data, vars=c("A", "B"), sample.nb=10, maxrepetition=c(1,1), print.info=FALSE),
    "Unknown column name"
  );
})

test_that("wrong_argument_sample.nb", {
  data <- data.frame(A=LETTERS[1:10], B=rep("un", 10), C=1:10)
  expect_error(
    balancedSampling(data, vars=c("A", "B"), sample.nb=3, maxrepetition=c(1,1), print.info=FALSE),
    "the number of items in the data frame is not a multiple of the requested number of samples."
  );
})

test_that("wrong_argument_values.nb", {
  data <- data.frame(A=LETTERS[1:10], B=rep("un", 10), C=1:10)
  expect_error(
    balancedSampling(data, vars=c("A", "B"), sample.nb=5, maxrepetition=c(1,1), print.info=FALSE),
    "the frequencies of the modalities in variable"
  );
})


test_that("wrong_argument_values.nb", {
  expect_equal(1, sample_consistent(1));
  expect_equal(33, sample_consistent(33));
  expect_equal("33", sample_consistent("33"));
  expect_equal("A", sample_consistent("A"));
  expected <- c(3, 76)
  expect_true(sample_consistent(expected) %in% expected);
  expected <- c("3", "76")
  expect_true(sample_consistent(expected) %in% expected);
})
