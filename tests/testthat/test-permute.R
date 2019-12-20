test_that("permute.without.repetition1", {
  #data.frame(A=LETTERS[1:10], B=sample(rep(1:2, 5)), C=rep(1:5, each=2))
  df <- data.frame(A=LETTERS[1:10], B=sample(rep(1:2, 5), 10), C=1)
  print(df)
  permute.without.repetition(df, "B")
})
