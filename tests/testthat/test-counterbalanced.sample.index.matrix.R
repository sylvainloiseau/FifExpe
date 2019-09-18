context("counterbalanced.sample.index.matrix")

test_that("counterbalanced.sample.index.matrix: feat1=20, feat2=5, sample.nb=5", {
  res <- counterbalanced.sample.index.matrix(20, 5, 5)

  frequency.by.row <- apply(res, 1, table)
  expect_true(all(frequency.by.row == 1))

  frequency.by.column <- apply(res, 2, table)
  expect_true(all(frequency.by.column == 4))
})

test_that("counterbalanced.sample.index.matrix: feat1=5, feat2=20, sample.nb=5", {
  res <- counterbalanced.sample.index.matrix(5, 20, 5)

  frequency.by.row <- apply(res, 1, table)
  expect_true(all(frequency.by.row == 4))

  frequency.by.column <- apply(res, 2, table)
  expect_true(all(frequency.by.column == 1))
})

test_that("counterbalanced.sample.index.matrix: feat1=40, feat2=5, sample.nb=5", {
  res <- counterbalanced.sample.index.matrix(40, 5, 5)

  frequency.by.row <- apply(res, 1, table)
  expect_true(all(frequency.by.row == 1))

  frequency.by.column <- apply(res, 2, table)
  expect_true(all(frequency.by.column == 8))
})

test_that("counterbalanced.sample.index.matrix: feat1=10, feat2=10, sample.nb=5", {
  res <- counterbalanced.sample.index.matrix(10, 10, 5)

  frequency.by.row <- apply(res, 1, table)
  expect_true(all(frequency.by.row == 2))

  frequency.by.column <- apply(res, 2, table)
  expect_true(all(frequency.by.column == 2))
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

