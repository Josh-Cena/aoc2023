quicksort <- function(x, compare) {
  n <- length(x)
  if (n <= 1) {
    return(x)
  }

  pivot <- x[[1]]
  less <- list()
  equal <- list(pivot)
  greater <- list()

  for (i in 2:n) {
    cmp <- compare(x[[i]], pivot)
    if (cmp < 0) {
      less[[length(less) + 1]] <- x[[i]]
    } else if (cmp > 0) {
      greater[[length(greater) + 1]] <- x[[i]]
    } else {
      equal[[length(equal) + 1]] <- x[[i]]
    }
  }

  c(quicksort(less, compare), equal, quicksort(greater, compare))
}

reduce <- function(x, f, init = NULL) {
  if (is.null(init)) {
    result <- x[[1]]
    start <- 2
  } else {
    result <- init
    start <- 1
  }
  for (i in start:length(x)) {
    result <- f(result, x[[i]])
  }
  return(result)
}
