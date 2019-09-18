#' Divide a set of items with two categorical variables into balanced samples.
#'
#' Items are lines of a data frame. Items have two properties: feat1 and feat2 (columns of the data frame).
#' The number of items is the cartesian product of the number of modalities of feat1 and feat2.
#'
#' The samples have the following properties:
#'
#' (1.) The item are exhaustively and without replacement divided into samples.
#' (2.) The modalities of feat1 and feat2 are equaly represented in each sample.
#' (3.) No more than 2 consecutive identical modalities in a sample.
#'
#' The properties 1 implies that the number of items is a multiple of the number of sample.
#' The properties 2 implies that the number of item of each modality in feat1 and feat2 is a mutiple of the number of sample.
#'
#' @param data a data frame, each line is an item.
#' @param feat1 the name of the column containing the feature 1 in the data frame.
#' @param feat2 the name of the column containing the feature 2 in the data frame.
#' @param sample.nb number of samples requested.
#'
#' @return A list of data frame.
#' @export
#'
#' @examples
#' data(dataFif)
#' samples <- double.condition.sampling(data=dataFif, "sent", "condition", 20)
counterbalanced.sampling <- function (data, feat1, feat2, sample.nb) {

    feat1.nb <- length(unique(data[[feat1]]))
    feat2.nb <- length(unique(data[[feat2]]))
    items.nb <- feat1.nb * feat2.nb
    sample.size <- items.nb / sample.nb

    # Assign each item to a sample with a balanced number of feat1 and feat2 in each sample
    samples.index <- counterbalanced.sample.index.matrix(feat1.nb, feat2.nb, sample.nb);

    samples <- split(data, as.vector(t(samples.index)));

    more.than.two.identical.consecutive.feat1 <- TRUE
    more.than.two.identical.consecutive.feat2 <- TRUE

    # While the sampling violate the condition, redo
    while (more.than.two.identical.consecutive.feat2 | more.than.two.identical.consecutive.feat1) {

      # shuffle the order of items in each
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
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Create a matrix of sample index without repetition of the index in a given row,
#' and with a balanced repetition of index in a given column.
#'
#' @param feat1.nb The number of row
#' @param feat2.nb The number of column
#' @param sample.nb The number of sample
#'
#' @return a matrix of numerics
#'
#' @examples
counterbalanced.sample.index.matrix <- function(feat1.nb, feat2.nb, sample.nb) {

  items.nb <- feat1.nb * feat2.nb
  sample.size <- items.nb / sample.nb;

  # Checking argument validity
  if ((items.nb %% sample.size) != 0)
    stop("The size of requested sample files is not a multiple of the number of items")
  if ((sample.size %% feat1.nb) != 0)
    stop("The sample size is not a multiple of the number of modality in feat1. The samples cannot be balanced.")
  if ((sample.size %% feat2.nb) != 0)
    stop("The sample size is not a multiple of the number of modality in feat2. The samples cannot be balanced.")

    square <- FALSE
    while (square[1]==FALSE) {
      square <- fill.counterbalanced.sample.index.matrix(feat1.nb, feat2.nb, sample.nb)
    }
    return(square)
  }

#' Try to fill a matrix. If we ends up into a dead end, with no more
#' logical solution, we give up and return FALSE
#'
#' @param feat1.nb
#' @param feat2.nb
#' @param sample.nb
#'
#' @return a matrix of numeric (sample index) or FALSE in case of a dead-end
#'
#' @examples
fill.counterbalanced.sample.index.matrix <- function(feat1.nb, feat2.nb, sample.nb) {
  items.nb <- feat1.nb * feat2.nb
  sample.size <- items.nb / sample.nb;
  square <- matrix(0, nrow=feat1.nb, ncol=feat2.nb);
  for (column in 1:feat2.nb) {
    for (row in 1:feat1.nb) {
      if (square[row,column] == 0 ) {

        not.available.in.row <- table(square[row,]) == (sample.size / feat1.nb)
        not.available.in.row <- as.numeric(names(not.available.in.row) [not.available.in.row])
        available.in.row <- setdiff(1:sample.nb, not.available.in.row)
        # available.in.row <- (1:sample.nb)[! 1:sample.nb %in% square[row,]]

        not.available.in.column <- table(square[,column]) == (sample.size / feat2.nb)
        not.available.in.column <- as.numeric(names(not.available.in.column) [not.available.in.column])
        available.in.column <- setdiff(1:sample.nb, not.available.in.column)

        possible.values <- intersect(available.in.row, available.in.column)
        if (length(possible.values) == 0) {
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
