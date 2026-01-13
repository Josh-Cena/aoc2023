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
      less <- push(less, x[[i]])
    } else if (cmp > 0) {
      greater <- push(greater, x[[i]])
    } else {
      equal <- push(equal, x[[i]])
    }
  }

  c(quicksort(less, compare), equal, quicksort(greater, compare))
}

push <- function(lst, value) {
  lst[[length(lst) + 1]] <- value
  lst
}
