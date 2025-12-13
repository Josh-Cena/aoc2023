tilt <- function(mat, dir) {
  m <- if (dir %in% c("N", "S")) nrow(mat) else ncol(mat)
  n <- if (dir %in% c("N", "S")) ncol(mat) else nrow(mat)
  for (i in seq_len(m)) {
    expected_pos <- if (dir %in% c("N", "W")) 1 else n
    for (j in if (dir %in% c("N", "W")) seq_len(n) else rev(seq_len(n))) {
      cur_ch <- if (dir %in% c("N", "S")) mat[j, i] else mat[i, j]
      if (cur_ch == "O") {
        if (dir %in% c("N", "S")) {
          mat[j, i] <- "."
          mat[expected_pos, i] <- "O"
        } else {
          mat[i, j] <- "."
          mat[i, expected_pos] <- "O"
        }
        expected_pos <- expected_pos + if (dir %in% c("N", "W")) 1 else -1
      } else if (cur_ch == "#") {
        expected_pos <- j + if (dir %in% c("N", "W")) 1 else -1
      }
    }
  }
  return(mat)
}

load <- function(mat) {
  total <- 0
  for (i in seq_len(nrow(mat))) {
    total <- total + sum(mat[i, ] == "O") * (nrow(mat) - i + 1)
  }
  return(total)
}

solve1 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  mat <- tilt(mat, "N")
  cat(load(mat), "\n")
}

base64_alphabet <- c(
  "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
  "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
  "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
  "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
  "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "+", "/"
)

# And of course, R has no hash maps, not even a matrix hasher...
# Plus the environment only supports key lengths up to 10000 bytes,
# and the matrix is 100x100, so we need to pack it
serialize_matrix <- function(mat) {
  flat <- as.vector(mat)
  flat <- c("." = 0, "O" = 1, "#" = 2)[flat]
  n <- length(flat)
  pad <- (3 - n %% 3) %% 3
  if (pad > 0) flat <- c(flat, rep(0, pad))
  raw <- matrix(flat, ncol = 3, byrow = TRUE)
  paste0(base64_alphabet[
    bitwOr(
      bitwOr(
        bitwShiftL(raw[, 1], 4),
        bitwShiftL(raw[, 2], 2)
      ),
      raw[, 3]
    )
  ], collapse = "")
}

solve2 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  seen <- new.env()
  its <- 0
  load <- list()
  repeat {
    key <- serialize_matrix(mat)
    load[[its + 1]] <- load(mat)
    if (!is.null(seen[[key]])) {
      last_it <- seen[[key]]
      period <- its - last_it
      break
    }
    seen[[key]] <- its
    mat <- tilt(mat, "N")
    mat <- tilt(mat, "W")
    mat <- tilt(mat, "S")
    mat <- tilt(mat, "E")
    its <- its + 1
  }
  ending_pos <- (1000000000 - last_it - 1) %% period + last_it + 1
  cat(load[[ending_pos + 1]], "\n")
}
