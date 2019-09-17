#' Divide a set of items into samples.
#'
#' Items are line of a data frame. Items have two properties: feat1 and feat2 (columns of the data frame).
#' The set of items is the cartesian product of feat1 and feat2.
#' The constraints on the sample are:
#' - Each item belongs to one and only one sample. The number of samples is then (nbr of items / sample size).
#' - Each sample contains only one representant of feat1. Then, the number of different feat1 cannot be lower than the number of sample, and the number of feat2 cannot be greater than the number of sample.
#' - No more than 2 consecutive identical feat1 modality in a sample.
#' - No more than 2 consecutive identical feat2 modality in a sample.
#' @param data a data frame, each line is an item.
#' @param feat1 the column name containing the feature 1.
#' @param feat2 the column name containing the feature 2.
#' @param sample.size size of the samples requested. The sample.size must be a multiple of the item number.
#'
#' @return A list of data frame.
#' @export
#'
#' @examples
#' sample.nb <- 5
#' data(dataFif)
#' samples <- double.condition.sampling(data=dataFif, "sent", "condition", 20)
counterbalanced.sampling <-
  function (data, feat1, feat2, sample.size) {

    feat1.nb <- length(unique(data[[feat1]]))
    feat2.nb <- length(unique(data[[feat2]]))
    items.nb <- feat1.nb * feat2.nb
    sample.nb <- items.nb / sample.size

    # Checking argument validity
    if ((items.nb %% sample.size) != 0)
      stop("The size of requested sample files is not a multiple of the number of items")
    if (feat1.nb < sample.size)
      stop("The sample size is too big, sentences cannot be unique in each sample.")
    if (feat2.nb > sample.nb)
      stop("The sampe size is too small, sentences cannot be unique in each sample.")

    more.than.two.identical.consecutive.feat1 <- TRUE
    more.than.two.identical.consecutive.feat2 <- TRUE

    # While the sampling violate the condition, redo
    while (more.than.two.identical.consecutive.feat2 | more.than.two.identical.consecutive.feat1) {

      # Assign each item to a sample with a balanced number of feat1 and feat2 in each sample
      samples.index <- counterbalanced.sample.index.matrix(feat1.nb, feat2.nb, sample.nb);

      samples <- split(data, as.vector(t(samples.index)));

      samples <- lapply(
        samples,
        function(x) x[sample(nrow(x), nrow(x)),]
      )

      # Test if the sampling violate the condition more.than.two.identical.consecutive.feat1
      string <- paste(unlist(lapply(samples, function(x) x[feat1]), use.names = F), collapse = "");
      more.than.two.identical.consecutive.feat1 <- is.value.repeated(string, feat1.nb)

      # Test if the sampling violate the condition more.than.two.identical.consecutive.feat2
      string <- paste(unlist(lapply(samples, function(x) x[feat2]), use.names = F), collapse = "");
      more.than.two.identical.consecutive.feat2 <- is.value.repeated(string, feat2.nb)
    }
    return(samples)
  }

is.value.repeated <- function(string, value.nb) {
  is.repeated <- FALSE
  for (i in 1:value.nb) {
    pat <- paste(rep(i, 3), collapse = "")
    found <- regexpr(pattern = pat, string)
    if (found[1] != -1) {
      # cat(paste(
      #   c("value ", i, " repeated: ", string, "\n"),
      #   collapse = ""
      # ))
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Create a matrix of sample index without repetition of the index in a row,
#' and with a balanced repetition of index in each column.
#'
#' @param feat1.nb The number of row
#' @param feat2.nb The number of column
#' @param sample.nb The number of sample
#'
#' @return a matrix of numerics
#'
#' @examples
counterbalanced.sample.index.matrix <- function(feat1.nb, feat2.nb, sample.nb) {
    feat2nb.by.sample <- feat1.nb / sample.nb;
    sample.size <- (feat1.nb * feat2.nb) / sample.nb;
    square <- FALSE
    while (square[1]==FALSE) {
      square <- try.counterbalanced.sample.index.matrix(feat1.nb, feat2.nb, sample.nb)
    }
    return(square)
  }

#' See counterbalanced.sample.index.matrix.
#'
#' @param feat1.nb
#' @param feat2.nb
#' @param sample.nb
#'
#' @return a matrix of numeric (sample index) or FALSE in case of a dead-end
#'
#' @examples
try.counterbalanced.sample.index.matrix <- function(feat1.nb, feat2.nb, sample.nb) {
  square <- matrix(0, nrow=feat1.nb, ncol=feat2.nb);
  for (column in sample(1:feat2.nb, feat2.nb)) {
    for (row in 1:feat1.nb) {
      if (square[row,column] == 0 ) {
        # print("---test---")
        # print(paste("row=", row))
        # print(paste("column=", column))
        available.in.row <- (1:sample.nb)[! 1:sample.nb %in% square[row,]]
        # print(available.in.row)
        not.available.in.column <- table(square[,column]) == (feat1.nb / sample.nb)
        not.available.in.column <- as.numeric(names(not.available.in.column) [not.available.in.column])
        available.in.column <- setdiff(1:sample.nb, not.available.in.column)
        # print(available.in.column)
        possible.values <- intersect(available.in.row, available.in.column)
        # print(possible.values)
        if (length(possible.values) == 0) {
          # print("======new trial")
          #print(square)
          return(FALSE)
        }
        if (length(possible.values) > 1) {
          square[row,column] <- sample(possible.values, 1);
        } else {
          square[row,column] <- possible.values
        }
      }
    }
  }
  return(square)
}

# add.margin.frequency <- function(m) {
#   by.rows <- t(apply(m, 1, table))
#   w.rows <- cbind(m, by.rows)
#   by.columns <- apply(w.rows, 2, table)
#   res <- rbind(w.rows, by.columns)
#   return(res)
# }
