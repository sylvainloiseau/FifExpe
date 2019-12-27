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
#' @param prop1 the name of the column containing the property 1 in the data frame.
#' @param prop2 the name of the column containing the property 2 in the data frame.
#' @param sample.nb number of samples requested.
#' @param maxrepetition NULL or a numeric vector giving the max allowed consecutive repetition of a modality for the two properties.
#' @return A list of data frame.
#' @export
#'
#' @examples
#' data(dataFif)
#' samples <- counterbalanced.sampling(data=dataFif, "sent", "condition", 20)
counterbalanced.sampling <- function (data, prop1, prop2, sample.nb, maxrepetition=c(2,2)) {
  if(! prop1 %in% colnames(data)) stop(paste("Unknown column name:", prop1));
  if(! prop2 %in% colnames(data)) stop(paste("Unknown column name:", prop2));

  if (!is.null(maxrepetition) & !is.numeric(maxrepetition))
    stop("'maxrepetition' must be a numeric vector");

  prop1.nb <- length(unique(data[[prop1]]))
  prop2.nb <- length(unique(data[[prop2]]))
  items.nb <- prop1.nb * prop2.nb
  sample.size <- items.nb / sample.nb

  cat("Sampling\n")
  # Assign each item to a sample with a balanced number of prop1 and prop2 in each sample
  samples.index <- counterbalanced.sample.index.matrix(data, prop1, prop2, prop1.nb, prop2.nb, sample.nb);

  ls <- vector(mode = "list", length=sample.nb)
  for (i in 1:sample.nb) {
    ls[[i]] <- data[samples.index[i,],]
  }
  #samples <- split(data, as.vector(t(samples.index)));

  # reorder in order to avoid repetition
  if (!is.null(maxrepetition)) {
    cat("Ordering\n")
    reordered <- lapply(ls, function(x) { order.without.repetition(x, c(prop1, prop2), maxrepetition) });
    # TODO ugly...
    while (reordered[[1]][[1]][1] == FALSE | reordered[[2]][[1]][1] == FALSE | reordered[[3]][[1]][1] == FALSE) {
      reordered <- lapply(ls, function(x) { order.without.repetition(x, c(prop1, prop2), maxrepetition)});
    }
    cat("\n")
  }
  cat("ok\n")
  return(reordered)
}

order.without.repetition <- function(sample.df, cols, maxrepetition) {
   if (length(cols) != length(maxrepetition)) stop("'cols' and 'maxrepetition' length must match.")
   index.ordered.wo.repetition <- c();
   sample.size <- nrow(sample.df)
   remaining <- 1:sample.size
   first.i <- sample(remaining, 1)
   index.ordered.wo.repetition[1] <- first.i;
   remaining <- remaining[-first.i];
   for (i in 2:sample.size) {
     possible <- remaining;
     for(j in 1:length(maxrepetition)) {
       col <- cols[j]
       maxrep <- maxrepetition[j]
       previous.i <- index.ordered.wo.repetition[max(1,i-maxrep):(i-1)];
       previous.val <- sample.df[previous.i, col]
       if (length(unique(previous.val) == maxrep)) {
         possible <- intersect(which(sample.df[, col] != previous.val[1]), possible);
       }
     }
     if(length(possible) == 0) {
       cat(".")
       return(list(data.frame(FALSE)));
     }
     found <- 0;
     if (length(possible) == 1) {
       found <- possible;
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
#' @param prop1.nb The number of row
#' @param prop2.nb The number of column
#' @param sample.nb The number of sample
#'
#' @return a matrix of numerics
#'
#' @examples
counterbalanced.sample.index.matrix <- function(data, prop1, prop2, prop1.nb, prop2.nb, sample.nb) {

  items.nb <- prop1.nb * prop2.nb
  sample.size <- items.nb / sample.nb;

  # Checking argument validity
  if ((items.nb %% sample.size) != 0)
    stop("The size of the requested samples is not a multiple of the number of items")
  if ((sample.size %% prop1.nb) != 0)
    stop("The sample size is not a multiple of the number of modality in prop1. The samples cannot be balanced.")
  if ((sample.size %% prop2.nb) != 0)
    stop("The sample size is not a multiple of the number of modality in prop2. The samples cannot be balanced.")

    square <- FALSE
    while (square[1]==FALSE) {
      square <- fill.counterbalanced.sample.index.matrix(data, prop1, prop2, prop1.nb, prop2.nb, sample.nb)
    }
    return(square)
  }

#' Try to fill a matrix. If we ends up into a dead end, with no more
#' logical solution, we give up and return FALSE
#'
#' @param prop1.nb
#' @param prop2.nb
#' @param sample.nb
#'
#' @return a matrix of numeric (sample index) or FALSE in case of a dead-end
#'
#' @examples
fill.counterbalanced.sample.index.matrix <- function(data, prop1, prop2, prop1.nb, prop2.nb, sample.nb) {
  items.nb <- prop1.nb * prop2.nb
  sample.size <- items.nb / sample.nb;
  square <- matrix(0, nrow=sample.nb, ncol=sample.size);

  remaining <- 1:items.nb;

  for (row in 1:sample.nb) {
    first.i <- sample(remaining, 1)
    square[row, 1] <- first.i;
    remaining <- remaining[-first.i];
    for (column in 2:sample.size) {
      # TODO ugly...
      prop1.max.occ.reached.t <- table(data[square[row,1:(column-1)], prop1]) == (sample.size / prop1.nb)
      prop1.max.occ.reached <- as.numeric(names(prop1.max.occ.reached.t) [prop1.max.occ.reached.t])
      prop2.max.occ.reached.t <- table(data[square[row,1:(column-1)], prop2]) == (sample.size / prop2.nb)
      prop2.max.occ.reached <- as.numeric(names(prop2.max.occ.reached.t) [prop2.max.occ.reached.t])

      not.available <- union(which(data[,prop1] %in% prop1.max.occ.reached), which(data[,prop2] %in% prop2.max.occ.reached));
      possible.values <- setdiff(remaining, not.available);

        if (length(possible.values) == 0) {
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
