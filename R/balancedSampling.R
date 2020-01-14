#' Divide a set of items into cross-balanced samples according to one or more qualitative variables. Each modalities
#' of the qualitative variables are equally distributed into the sample.
#'
#' Items are the lines of a data frame. Qualitative variables are columns.
#'
#' @param data a data frame.
#' The items (row) must be the cartesian product of the modalities of the qualitative variables in 'vars',
#' or a multiple of this cartesian product.
#' @param vars the names of the columns containing the qualitative variables to be balanced between the samples. Each modalities
#' of each vars must be equals to the 'sample.nb', or a multiple of 'sample.nb'.
#' @param sample.nb number of samples requested.
#' @param maxrepetition NULL or a numeric vector (same length as 'vars') giving the max allowed consecutive repetition of a modality for the qualitative variables
#' @param print.info print information to stdout.
#' @return A list of data frame.
#' @export
#' @examples
#' data(dataFif)
#' samples <- counterbalanced.sampling(data=dataFif, "sent", "condition", 20)
balancedSampling <- function (data, vars, sample.nb, maxrepetition=c(2,2), print.info=TRUE) {
  for(var in vars) {
    if(! var %in% colnames(data)) stop(paste("Unknown column name: '", var, "'", sep=""));
  }

  if (!is.null(maxrepetition) & !is.numeric(maxrepetition))
    stop("'maxrepetition' must be a numeric vector");
  if (!length(maxrepetition) == length(vars))
    stop("'maxrepetition' length must be identical to the length of 'vars'");

  if (print.info) cat("Sampling\n")
  # Assign each item to a sample with a balanced number of prop1 and prop2 in each sample
  samples.index <- counterbalanced.sample.index.matrix(data, vars, sample.nb, print.info);

  ls <- vector(mode = "list", length=sample.nb)
  for (i in 1:sample.nb) {
    ls[[i]] <- data[samples.index[i,],]
  }

  # reorder in order to avoid repetition
  reordered <- vector(mode="list", length=sample.nb);
  if (!is.null(maxrepetition)) {
    if (print.info) cat("Ordering\n")
    for (i in 1:sample.nb) {
      reordered[[i]] <- order.without.repetition(ls[[i]], vars, maxrepetition, print.info);
    }
    if (print.info) cat("\n")
  } else {
    reordered <- ls
  }
  cat("ok\n")
  return(reordered)
}

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
order.without.repetition <- function(data, cols, maxrepetition, maxtry=50, print.info=TRUE) {
  ntry <- 0;
  res <- try.order.without.repetition(data, cols, maxrepetition);
  while (is.logical(res)) {
    if (print.info) cat(".")
    if (ntry >= maxtry) {
      if (print.info) cat("\n")
      stop("the maximum number of tries has been reached")
    }
    res <- try.order.without.repetition(data, cols, maxrepetition);
    ntry <- ntry + 1
  }
  return(res)
}

#' [Private]
try.order.without.repetition <- function(data, cols, maxrepetition) {
   if (length(cols) != length(maxrepetition)) stop("'cols' and 'maxrepetition' length must match.")
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

#' [Private]
#' Create a matrix of sample index without repetition of the index in a given row,
#' and with a balanced repetition of index in a given column.
#'
#' @param vars the names of the columns containing the qualitative variables to be balanced between the samples.
#' @param sample.nb The number of sample
#'
#' @return a matrix of numerics
#'
#' @examples
counterbalanced.sample.index.matrix <- function(data, vars, sample.nb, print.info, maxtry=50) {

  items.nb <- nrow(data);
  if ((items.nb %% sample.nb) != 0) stop("the number of items in the data frame is not a multiple of the requested number of samples.")

  # for each variable, the number of modality
  nb.modalities <- vector(mode = "integer", length=length(vars))
  # for each variable, the frequency of its modalities (each modalities of a variable must have the same frequency)
  all.frequencies <- vector(mode = "integer", length=length(vars))
  for (i in 1:length(vars)) {
    nb.modalities[i] <- length(unique(data[[vars[i]]]));
    frequencies <- table(data[[vars[i]]]);
    frequency <- frequencies[i];
    if (! all(frequencies == frequency)) stop(paste("the modalities in variable '", vars[i], "' does not all have the same frequency.", sep=""));
    if ((frequency %% sample.nb) != 0 ) stop(paste("the frequencies of the modalities in variable '", vars[i], "' (", frequency, ") is not a multiple of the number of samples (", sample.nb, ").", sep=""));
    all.frequencies[i] <- frequency;
  }

  prods <- cumprod(nb.modalities);
  cartesian.product <- prods[length(prods)];
  if ((items.nb %% cartesian.product) != 0) stop("the number of items in the data frame is not a multiple of the product of the number of modalities of the qualitative variables")

  #combination.frequencies <- array(0, dim=nb.modalities, dimnames=vars);
  #for (i in 1:nrow(data)) {
  #  combination.frequencies[data[i,vars]]
  #}

  sample.size <- items.nb / sample.nb;

  ntry <- 0;
  newdata <- FALSE;
    while (newdata[1]==FALSE) {
      if (print.info) cat(".")
      if (ntry >= maxtry) {
        stop("the maximum number of tries has been reached")
      }
      newdata <- fill.counterbalanced.sample.index.matrix(data, vars, nb.modalities, all.frequencies, cartesian.product, sample.nb, sample.size)
      ntry <- ntry + 1
    }
  if (print.info) cat("\n")
  return(newdata)
}

#' [Private]
#' Try to fill a matrix. If we ends up into a dead end, with no more
#' logical solution, we give up and return FALSE
#'
#' @param vars
#' @param nb.modalities
#' @param nb.values
#' @param cartesian.product
#' @param sample.nb
#' @param sample.size
#'
#' @return a matrix of numeric (sample index) or FALSE in case of a dead-end
#'
#' @examples
fill.counterbalanced.sample.index.matrix <- function(data, vars, nb.modalities, nb.values, cartesian.product, sample.nb, sample.size) {
  items.nb <- nrow(data);
  newdata <- matrix(0, nrow=sample.nb, ncol=sample.size);

  remaining <- 1:items.nb;

  for (row in 1:sample.nb) {
    first.i <- sample_consistent(remaining);
    newdata[row, 1] <- first.i;
    remaining <- remaining[-first.i];
    for (column in 2:sample.size) {
      not.available <- vector(mode="character", length=0);
      for (i in 1:length(vars)) {
        var <- vars[i];
        freqs <- table(data[newdata[row,1:(column-1)], var])
        is.max.occ.reached <- freqs == (nb.values[i] / sample.nb);
        named.max.occ.reached <- names(is.max.occ.reached) [is.max.occ.reached];
        not.available <- union(not.available, which(data[,vars[i]] %in% named.max.occ.reached))
      }
      possible.values <- setdiff(remaining, not.available);
      if (length(possible.values) == 0) {
        return(FALSE)
      }
      i <- sample_consistent(possible.values)
      newdata[row,column] <- i;
      remaining <- remaining[-which(remaining == i)];
    }
  }
  return(newdata)
}

#' the base R sample function behave diffently according to the length of the first element (the set).
sample_consistent <- function (valueset) {
  res <- vector(mode=mode(valueset), length=1);
  if (length(valueset) > 1) {
    res <- sample(valueset, 1);
  } else {
    res <- valueset
  }
  return(res)
}
