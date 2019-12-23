#' Divide a set of items with two categorical variables into cross-balanced samples.
#'
#' Items are lines of a data frame. Items have at least two categorial properties: prop1 and prop2 (columns of the data frame).
#' The number of items is the product of the number of modalities of prop1 and prop2.
#'
#' The sampling has the following properties:
#'
#' (1.) The items are exhaustively and without replacement divided into samples.
#' (2.) The modalities of prop1 and prop2 are equally represented in each sample.
#' (3.) Optionaly, no more than 2 consecutive identical modalities in a sample.
#'
#'
#' @param data a data frame, each line is an item.
#' @param feat1 the name of the column containing the property 1 in the data frame.
#' @param feat2 the name of the column containing the property 2 in the data frame.
#' @param sample.nb number of samples requested.
#' @param norepetition NULL or a vector containing either feat1, feat2, or both. Those modality will not be repeated in the sample.
#' @return A list of data frame.
#' @export
#'
#' @examples
#' data(dataFif)
#' samples <- counterbalanced.sampling(data=dataFif, "sent", "condition", 20)
counterbalanced.sampling <- function (data, feat1, feat2, sample.nb, norepetition=c(feat1, feat2)) {
  if(! feat1 %in% colnames(data)) stop(paste("Unknown column name:", feat1));
  if(! feat2 %in% colnames(data)) stop(paste("Unknown column name:", feat2));

  if (!is.null(norepetition) & !all(norepetition %in% c(feat1, feat2)))
    stop("The properties in norepetition must be eater feat1 or feat2 or both");

  feat1.nb <- length(unique(data[[feat1]]))
  feat2.nb <- length(unique(data[[feat2]]))
  items.nb <- feat1.nb * feat2.nb
  sample.size <- items.nb / sample.nb

  # Assign each item to a sample with a balanced number of feat1 and feat2 in each sample
  samples.index <- counterbalanced.sample.index.matrix(data, feat1, feat2, feat1.nb, feat2.nb, sample.nb);

  ls <- vector(mode = "list", length=sample.nb)
  for (i in 1:sample.nb) {
    ls[[i]] <- data[samples.index[i,],]
  }
  #samples <- split(data, as.vector(t(samples.index)));

  # reorder in order to avoid repetition
  if (!is.null(norepetition)) {
    reordered <- lapply(ls, function(x) { order.without.repetition(x, norepetition)});
    while (reordered[[1]][[1]][1] == FALSE | reordered[[2]][[1]][1] == FALSE | reordered[[3]][[1]][1] == FALSE) {
      reordered <- lapply(ls, function(x) { order.without.repetition(x, norepetition)});
    }
  }
  cat("ok\n")
  return(reordered)
}

order.without.repetition <- function(sample.df, cols) {
   index.ordered.wo.repetition <- c();
   sample.size <- nrow(sample.df)
   remaining <- 1:sample.size
   first.i <- sample(remaining, 1)
   index.ordered.wo.repetition[1] <- first.i;
   remaining <- remaining[-first.i];
   for (i in 2:sample.size) {
     previous.i <- index.ordered.wo.repetition[i-1];
     possible <- remaining;
     for(col in cols) {
       possible <- intersect(which(sample.df[, col] != sample.df[previous.i, col]), possible);
     }
     if(length(possible) == 0) {
       cat("No more reordering possibility. Resuming...\n")
       return(list(data.frame(FALSE)));
     }
       #stop("No more logical possibility.")
     found <- 0;
     if (length(possible) == 1) {
       found = possible;
     } else {
       found <- sample(possible, 1)
     }
     index.ordered.wo.repetition[i] <- found
     remaining <- remaining[-which(remaining == found)];
   }
   if (length(index.ordered.wo.repetition) != sample.size) stop("The ordering index does not fit the length of the data.")
   res <- sample.df[index.ordered.wo.repetition,]
   return(res)
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
counterbalanced.sample.index.matrix <- function(data, feat1, feat2, feat1.nb, feat2.nb, sample.nb) {

  items.nb <- feat1.nb * feat2.nb
  sample.size <- items.nb / sample.nb;

  # Checking argument validity
  if ((items.nb %% sample.size) != 0)
    stop("The size of the requested samples is not a multiple of the number of items")
  if ((sample.size %% feat1.nb) != 0)
    stop("The sample size is not a multiple of the number of modality in feat1. The samples cannot be balanced.")
  if ((sample.size %% feat2.nb) != 0)
    stop("The sample size is not a multiple of the number of modality in feat2. The samples cannot be balanced.")

    square <- FALSE
    while (square[1]==FALSE) {
      square <- fill.counterbalanced.sample.index.matrix(data, feat1, feat2, feat1.nb, feat2.nb, sample.nb)
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
fill.counterbalanced.sample.index.matrix <- function(data, feat1, feat2, feat1.nb, feat2.nb, sample.nb) {
  items.nb <- feat1.nb * feat2.nb
  sample.size <- items.nb / sample.nb;
  square <- matrix(0, nrow=sample.nb, ncol=sample.size);

  remaining <- 1:items.nb;

  for (row in 1:sample.nb) {
    first.i <- sample(remaining, 1)
    square[row, 1] <- first.i;
    remaining <- remaining[-first.i];
    for (column in 2:sample.size) {
      feat1.max.occ.reached.t <- table(data[square[row,1:column-1], feat1]) == (sample.size / feat1.nb)
      feat1.max.occ.reached <- as.numeric(names(feat1.max.occ.reached.t) [feat1.max.occ.reached.t])
      feat2.max.occ.reached.t <- table(data[square[row,1:column-1], feat2]) == (sample.size / feat2.nb)
      feat2.max.occ.reached <- as.numeric(names(feat2.max.occ.reached.t) [feat2.max.occ.reached.t])

      not.available <- union(which(data[,feat1] %in% feat1.max.occ.reached), which(data[,feat2] %in% feat2.max.occ.reached));
      possible.values <- setdiff(remaining, not.available);

        if (length(possible.values) == 0) {
          # print(square)
          # print(feat1.max.occ.reached)
          # print(feat1.max.occ.reached.t)
          # print(feat2.max.occ.reached)
          # print(feat2.max.occ.reached.t)
          # print(remaining)
          # print(not.available)

          cat(".")
          return(FALSE)
        }
        i <- 0
        if (length(possible.values) > 1) {
          i <- sample(possible.values, 1);
        } else {
          i <- possible.values
        }
        square[row,column] <- i;
      remaining <- remaining[-which(remaining == i)];
    }
  }
  cat("\n")
  return(square)
}
