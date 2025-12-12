num_matches <- function(line) {
  winning <- line$winning
  numbers <- line$numbers
  length(numbers[numbers %in% winning])
}

solve1 <- function(data) {
  lines <- sapply(strsplit(data, ": +"), function(x) x[2])
  lines <- lapply(lines, function(x) {
    halves <- strsplit(x, " +\\| +")[[1]]
    return(list(
      winning = as.numeric(strsplit(halves[1], " +")[[1]]),
      numbers = as.numeric(strsplit(halves[2], " +")[[1]])
    ))
  })
  scores <- sapply(lines, num_matches)
  scores <- floor(2^(scores - 1))
  cat(sum(scores), "\n")
}

solve2 <- function(data) {
  lines <- sapply(strsplit(data, ": +"), function(x) x[2])
  lines <- lapply(lines, function(x) {
    halves <- strsplit(x, " +\\| +")[[1]]
    return(list(
      winning = as.numeric(strsplit(halves[1], " +")[[1]]),
      numbers = as.numeric(strsplit(halves[2], " +")[[1]])
    ))
  })
  scores <- sapply(lines, num_matches)
  counts <- rep(1, length(lines))
  for (i in 1:length(scores)) {
    if (scores[i] > 0) {
      for (j in (i + 1):(i + scores[i])) {
        counts[j] <- counts[j] + counts[i]
      }
    }
  }
  cat(sum(counts), "\n")
}
