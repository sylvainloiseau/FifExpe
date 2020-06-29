#' Divide a set of items into cross-balanced or counter-balanced samples according to
#' one or more qualitative variables. Each modalities of the qualitative variables are
#' equally distributed into the sample.
#'
#' Items are the lines of a data frame. Qualitative variables are columns.
#'
#' @param data a data frame.
#' The items (row) must be the cartesian product of the modalities of the qualitative variables in 'vars',
#' or a multiple of this cartesian product.
#' @param vars the names of the columns containing the qualitative variables to be balanced between the samples.
#' Each modalities of each vars must be equals to the 'sample.nb', or a multiple of 'sample.nb'.
#' @param sample.nb number of samples requested.
#' @param maxrepetition NULL or a numeric vector (same length as 'vars') giving the max allowed
#' consecutive repetition of a modality for the qualitative variables
#' @param print.info print information to stdout.
#' @return A list of data frame.
#' @export
#' @examples
#' data(fif3con)
#' samples <- balancedSampling(data=fif3con, c("sent", "condition"), 3)
balancedSampling <- function (data, vars, sample.nb, maxrepetition=NULL, print.info=TRUE) {
  for(var in vars) {
    if(! var %in% colnames(data)) stop(paste("Unknown column name: '", var, "'", sep=""));
  }

  if (!is.null(maxrepetition) & !is.numeric(maxrepetition))
    stop("'maxrepetition' must be a numeric vector");
  if (!is.null(maxrepetition) & !length(maxrepetition) == length(vars))
    stop("'maxrepetition' length must be identical to the length of 'vars'");

  if (print.info) cat("Sampling\n")
  # Assign each item to a sample with a balanced number of prop1 and prop2 in each sample
  samples.index <- counterbalanced.sample.index.matrix(data, vars, sample.nb, print.info);

  ls <- vector(mode = "list", length=sample.nb)
  for (i in 1:sample.nb) {
    ls[[i]] <- data[samples.index[i,],,drop=FALSE]
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

#' [Private]
#' Create a matrix of sample index without repetition of the index in a given row,
#' and with a balanced repetition of index in a given column.
#'
#' @param data the data frame
#' @param vars the qualitative variables (columns of 'data')
#' @param sample.nb the number of samples
#' @param print.info are information printed on std.out?
#' @param maxtry maximum number of tries
#'
#' @return a matrix of numerics
counterbalanced.sample.index.matrix <- function(data, vars, sample.nb, print.info, maxtry=100) {

  items.nb <- nrow(data);
  if ((items.nb %% sample.nb) != 0)
    stop(paste("the number of items (rows) in 'data' (", items.nb, ") is not a multiple of the requested number of samples (", sample.nb,").", sep=""));

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

  # expand.grid
  #cartesian.product <- array(0, dim=all.frequencies?, dimnames=vars);
  #for (i in 1:nrow(data)) {
  #  cartesian.product[data[i,vars]]
  #}

  sample.size <- items.nb / sample.nb;

  ntry <- 0;
  newdata <- FALSE;
    while (newdata[1]==FALSE) {
      if (print.info) cat(".")
      if (ntry >= maxtry) {
        stop("the maximum number of tries has been reached")
      }
      newdata <- try.counterbalanced.sample.index.matrix(data, vars, nb.modalities, all.frequencies, cartesian.product, sample.nb, sample.size)
      ntry <- ntry + 1
    }
  if (print.info) cat("\n")
  return(newdata)
}

#' [Private]
#' Try to fill a matrix. If we ends up into a dead end, with no more
#' logical solution, we give up and return FALSE
#'
#' @param data the data frame
#' @param vars the qualitative variables (columns of 'data')
#' @param nb.modalities for each qualitative variable, its number of modalities.
#' @param nb.values for each qualitative variables, the frequency of its modalities (frequency must be the same for all modality of a variable).
#' @param cartesian.product cartesian product of the number of modalities of each variable. The number of item must be identical to this number, or a product of it.
#' @param sample.nb the number of samples
#' @param sample.size size of the samples
#'
#' @return a matrix of numeric (sample index) or FALSE in case of a dead-end
#'
try.counterbalanced.sample.index.matrix <- function(data, vars, nb.modalities, nb.values, cartesian.product, sample.nb, sample.size) {
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

#' [private]
#' Randomly draw one element from a vector of one or more elements.
#'
#' the base R sample function behave diffently according to the length of the first element: if it is a length-1 numeric vector, it is interpreted as the size of the urn, not as the only element to draw.
#'
#' @param valueset set of elements
#'
#' @return a length-1 vector containing one element lf valueset
sample_consistent <- function (valueset) {
  res <- vector(mode=mode(valueset), length=1);
  if (length(valueset) > 1) {
    res <- sample(valueset, 1);
  } else {
    res <- valueset
  }
  return(res)
}
