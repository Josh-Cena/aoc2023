solve1 <- function(data) {
  sum <- 0
  for (i in seq_along(data)) {
    match_res <- gregexpr("[0-9]+", data[i])
    matches <- match_res[[1]]
    if (matches[1] == -1) {
      next
    }
    starts <- as.integer(matches)
    values <- regmatches(data[i], match_res)[[1]]
    lengths <- nchar(values)
    for (j in seq_along(starts)) {
      l <- starts[j] - 1
      r <- starts[j] + lengths[j]
      neighbors <- paste0(
        substr(data[i], l, l),
        substr(data[i], r, r),
        if (i > 1) substr(data[i - 1], l, r) else "",
        if (i < length(data)) substr(data[i + 1], l, r) else ""
      )
      if (grepl("[^0-9.]", neighbors)) {
        sum <- sum + as.integer(values[j])
      }
    }
  }
  cat(sum, "\n")
}

key <- function(i, j) {
  return(paste0(i, ",", j))
}

solve2 <- function(data) {
  neighbors <- list()
  add_neighbor <- function(neighbors, gear_pos, value) {
    if (gear_pos %in% names(neighbors)) {
      neighbors[[gear_pos]] <-
        c(neighbors[[gear_pos]], value)
    } else {
      neighbors[[gear_pos]] <- value
    }
    neighbors
  }
  for (i in seq_along(data)) {
    match_res <- gregexpr("[0-9]+", data[i])
    matches <- match_res[[1]]
    if (matches[1] == -1) {
      next
    }
    starts <- as.integer(matches)
    values <- regmatches(data[i], match_res)[[1]]
    lengths <- nchar(values)
    for (j in seq_along(starts)) {
      l <- starts[j] - 1
      r <- starts[j] + lengths[j]
      if (substr(data[i], l, l) == "*") {
        neighbors <-
          add_neighbor(neighbors, key(i, l), as.integer(values[j]))
      }
      if (substr(data[i], r, r) == "*") {
        neighbors <-
          add_neighbor(neighbors, key(i, r), as.integer(values[j]))
      }
      if (i > 1) {
        above <- substr(data[i - 1], l, r)
        matches <- gregexpr("\\*", above)[[1]]
        if (matches[1] != -1) {
          for (k in matches) {
            # If l == 0, then we need to add 1 to k to compensate for the
            # leftmost lost character in the above string
            gear_pos <- key(i - 1, l + k - min(l, 1))
            neighbors <-
              add_neighbor(neighbors, gear_pos, as.integer(values[j]))
          }
        }
      }
      if (i < length(data)) {
        below <- substr(data[i + 1], l, r)
        matches <- gregexpr("\\*", below)[[1]]
        if (matches[1] != -1) {
          for (k in matches) {
            gear_pos <- key(i + 1, l + k - min(l, 1))
            neighbors <-
              add_neighbor(neighbors, gear_pos, as.integer(values[j]))
          }
        }
      }
    }
  }
  sum <- sum(sapply(neighbors, function(x) if (length(x) == 2) prod(x) else 0))
  cat(sum, "\n")
}
