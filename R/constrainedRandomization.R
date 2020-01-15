#' Reorder the lines of a data frame so that there is no more
#' that the given repetition of a modality in the given columns.
#'
#' @param data a data frame
#'
#' @param cols a character vector giving the name of the column
#' of the data frame for which constraint on the repetition have to be observed.
#'
#' @param maxrepetition a numeric vector of the same length
#' than 'cols' giving the the maximum consecutive identical modality allowed.
#'
#' @param print.info print information to stdout.
#'
#' @return the same data frame as 'data', with the lines reordered
#'
#' @export
#'
#' @examples
#'   data <- data.frame(
#'     A=c(rep("a", 2), rep("b", 2)),
#'     B=c(rep("c", 2), rep("d", 2))
#'   );
#'   order.without.repetition(data, c("A", "B"), c(1,1));
order.without.repetition <- function(data, cols, maxrepetition, maxtry=10, print.info=TRUE) {
  if (length(cols) != length(maxrepetition)) stop("'cols' and 'maxrepetition' length must match.")
  ntry <- 0;
  res <- try.order.without.repetition(data, cols, maxrepetition);
  while (is.logical(res)) {
    if (print.info) cat(".")
    if (ntry >= maxtry) {
      if (print.info) cat("\n")
      stop(paste("the maximum number of tries (", maxtry, ") has been reached", sep=""));
    }
    res <- try.order.without.repetition(data, cols, maxrepetition);
    ntry <- ntry + 1
  }
  return(res)
}

#' [Private]
try.order.without.repetition <- function(data, cols, maxrepetition) {
  index.ordered.wo.repetition <- c();
  sample.size <- nrow(data)
  remaining <- 1:sample.size

  # the first element
  first.i <- sample_consistent(remaining)
  index.ordered.wo.repetition[1] <- first.i;
  remaining <- remaining[-first.i];

  # the remaining elements
  for (i in 2:sample.size) {
    possible <- remaining;
    for(j in 1:length(maxrepetition)) {
      col <- cols[j]
      maxrep <- maxrepetition[j]
      previous.i <- index.ordered.wo.repetition[max(1,i-maxrep):(i-1)];
      previous.val <- data[previous.i, col]
      if (length(unique(previous.val) == 1)) {
        possible <- intersect(which(data[, col] != previous.val[1]), possible);
      }
    }
    if(length(possible) == 0) {
      return(FALSE);
    }
    found <- sample_consistent(possible);
    index.ordered.wo.repetition[i] <- found
    remaining <- remaining[-which(remaining == found)];
  }
  if (length(index.ordered.wo.repetition) != sample.size) stop("The ordering index does not fit the length of the data.")
  res <- data[index.ordered.wo.repetition,]
  return(res)
}
