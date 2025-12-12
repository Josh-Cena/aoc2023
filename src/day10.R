trace_loop <- function(mat) {
  start <- which(mat == "S", arr.ind = TRUE)
  start <- c(start[2], start[1])
  dir <- if (mat[start[2], start[1] + 1] %in% c("-", "J", "7")) {
    c(1, 0)
  } else if (mat[start[2], start[1] - 1] %in% c("-", "F", "L")) {
    c(-1, 0)
  } else if (mat[start[2] + 1, start[1]] %in% c("|", "L", "J")) {
    c(0, 1)
  } else if (mat[start[2] - 1, start[1]] %in% c("|", "F", "7")) {
    c(0, -1)
  } else {
    stop("Invalid start")
  }
  is_loop <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  is_loop[start[2], start[1]] <- TRUE
  cur <- start + dir
  n <- 1
  while (!all(cur == start)) {
    is_loop[cur[2], cur[1]] <- TRUE
    dir <- if (mat[cur[2], cur[1]] %in% c("-", "|")) {
      dir
    } else if (mat[cur[2], cur[1]] == "L") {
      if (dir[1] == 0) c(1, 0) else c(0, -1)
    } else if (mat[cur[2], cur[1]] == "J") {
      if (dir[1] == 0) c(-1, 0) else c(0, -1)
    } else if (mat[cur[2], cur[1]] == "7") {
      if (dir[1] == 0) c(-1, 0) else c(0, 1)
    } else if (mat[cur[2], cur[1]] == "F") {
      if (dir[1] == 0) c(1, 0) else c(0, 1)
    } else {
      stop("Invalid move")
    }
    cur <- cur + dir
    n <- n + 1
  }
  return(is_loop)
}

solve1 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  is_loop <- trace_loop(mat)
  n <- sum(is_loop)
  cat(n / 2, "\n")
}

solve2 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  is_loop <- trace_loop(mat)
  enclosed <- 0
  for (i in 1:nrow(mat)) {
    inside <- FALSE
    j <- 1
    while (j <= ncol(mat)) {
      if (is_loop[i, j]) {
        if (mat[i, j] %in% c("|")) {
          inside <- !inside
        } else if (mat[i, j] == "L") {
          j <- j + 1
          while (mat[i, j] == "-") {
            j <- j + 1
          }
          if (mat[i, j] == "7") {
            inside <- !inside
          }
        } else if (mat[i, j] == "F") {
          j <- j + 1
          while (mat[i, j] == "-") {
            j <- j + 1
          }
          if (mat[i, j] == "J") {
            inside <- !inside
          }
        }
      } else if (inside) {
        enclosed <- enclosed + 1
      }
      j <- j + 1
    }
  }
  cat(enclosed, "\n")
}
