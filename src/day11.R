solve <- function(data, expansion) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  empty_rows <- which(apply(mat, 1, function(x) all(x == ".")))
  empty_cols <- which(apply(mat, 2, function(x) all(x == ".")))
  galaxies <- which(mat == "#", arr.ind = TRUE)
  sum <- 0
  for (i in 1:(nrow(galaxies) - 1)) {
    for (j in (i + 1):nrow(galaxies)) {
      g1y <- galaxies[i, 1]
      g1x <- galaxies[i, 2]
      g2y <- galaxies[j, 1]
      g2x <- galaxies[j, 2]
      crossed_empty_rows <-
        sum(empty_rows > min(g1y, g2y) & empty_rows < max(g1y, g2y))
      crossed_empty_cols <-
        sum(empty_cols > min(g1x, g2x) & empty_cols < max(g1x, g2x))
      distance <- abs(g1y - g2y) + abs(g1x - g2x)
        + ((crossed_empty_rows + crossed_empty_cols) * (expansion - 1))
      sum <- sum + distance
    }
  }
  cat(sum, "\n")
}

solve1 <- function(data) {
  solve(data, 2)
}
solve2 <- function(data) {
  solve(data, 1000000)
}
