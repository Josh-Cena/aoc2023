library("gmp", include.only = c("as.bigz"))

count_dp <- function(cur_springs, expected_counts) {
  m <- length(expected_counts)
  max_seg_len <- max(expected_counts)

  # For each spring, number of ways to reach each state (j,seg_len),
  # which are the index and length of the current segment.
  # If no segment is active, we're at state (j,0).
  dp <- matrix(0, nrow = m + 1, ncol = max_seg_len + 1)
  dp[1, 1] <- 1 # Start: j=1, seg_len=0

  for (ch in cur_springs) {
    next_dp <- matrix(0, nrow = m + 1, ncol = max_seg_len + 1)

    for (j in 1:(m + 1)) {
      for (seg_len in 0:max_seg_len) {
        ways <- dp[j, seg_len + 1]
        if (ways == 0) next # Unreachable state
        if (ch == "." || ch == "?") {
          if (j <= m && seg_len == expected_counts[j]) {
            # Close this segment. If length doesn't match, dp=0
            next_dp[j + 1, 0 + 1] <- next_dp[j + 1, 0 + 1] + ways
          } else if (seg_len == 0) {
            # Stay outside a segment
            next_dp[j, 0 + 1] <- next_dp[j, 0 + 1] + ways
          }
        }
        if (ch == "#" || ch == "?") {
          # Can only stay in a segment if it's not too long
          if (j <= m && seg_len < expected_counts[j]) {
            next_dp[j, seg_len + 1 + 1] <- next_dp[j, seg_len + 1 + 1] + ways
          }
        }
      }
    }

    dp <- next_dp
  }

  # At the end, we're either in an idle state (seg_len=0) or
  # at the end of a segment of the expected length
  dp[m + 1, 0 + 1] + dp[m, expected_counts[m] + 1]
}

solve1 <- function(data) {
  parts <- strsplit(data, " ")
  springs <- sapply(parts, function(x) strsplit(x[1], "")[[1]])
  expected_counts <-
    lapply(parts, function(x) as.numeric(strsplit(x[2], ",")[[1]]))
  total <- 0
  for (i in seq_along(springs)) {
    pos <- count_dp(springs[[i]], expected_counts[[i]])
    total <- total + pos
  }
  cat(total, "\n")
}

duplicate_springs <- function(springs) {
  new_springs <- c()
  for (i in 1:5) {
    new_springs <- c(new_springs, springs)
    if (i < 5) {
      new_springs <- c(new_springs, "?")
    }
  }
  return(new_springs)
}

solve2 <- function(data) {
  parts <- strsplit(data, " ")
  springs <-
    sapply(parts, function(x) duplicate_springs(strsplit(x[1], "")[[1]]))
  expected_counts <-
    lapply(parts, function(x) rep(as.numeric(strsplit(x[2], ",")[[1]]), 5))
  total <- as.bigz(0)
  for (i in seq_along(springs)) {
    pos <- count_dp(springs[[i]], expected_counts[[i]])
    total <- total + pos
  }
  cat(as.character(total), "\n")
}
