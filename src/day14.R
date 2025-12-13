solve1 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  total <- 0
  for (i in seq_len(ncol(mat))) {
    col <- mat[, i]
    expected_pos <- 1
    n <- length(col)
    for (j in seq_len(n)) {
      if (col[j] == "O") {
        total <- total + n - expected_pos + 1
        expected_pos <- expected_pos + 1
      } else if (col[j] == "#") {
        expected_pos <- j + 1
      }
    }
  }
  cat(total, "\n")
}
