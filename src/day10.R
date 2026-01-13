trace_loop <- function(mat) {
  start <- which(mat == "S", arr.ind = TRUE)
  dir <- if (mat[start[1], start[2] + 1] %in% c("-", "J", "7")) {
    c(1, 0)
  } else if (mat[start[1], start[2] - 1] %in% c("-", "F", "L")) {
    c(-1, 0)
  } else if (mat[start[1] + 1, start[2]] %in% c("|", "L", "J")) {
    c(0, 1)
  } else if (mat[start[1] - 1, start[2]] %in% c("|", "F", "7")) {
    c(0, -1)
  } else {
    stop("Invalid start")
  }
  init_dir <- dir
  cur <- start + dir
  corners <- list()
  while (!all(cur == start)) {
    if (mat[cur[1], cur[2]] %in% c("L", "J", "7", "F")) {
      corners <- push(corners, cur)
    }
    dir <- if (mat[cur[1], cur[2]] %in% c("-", "|")) {
      dir
    } else if (mat[cur[1], cur[2]] == "L") {
      if (dir[1] == 0) c(-1, 0) else c(0, 1)
    } else if (mat[cur[1], cur[2]] == "J") {
      if (dir[1] == 0) c(-1, 0) else c(0, -1)
    } else if (mat[cur[1], cur[2]] == "7") {
      if (dir[1] == 0) c(1, 0) else c(0, -1)
    } else if (mat[cur[1], cur[2]] == "F") {
      if (dir[1] == 0) c(1, 0) else c(0, 1)
    } else {
      stop("Invalid move")
    }
    cur <- cur + dir
  }
  if (!all(dir == init_dir)) {
    # If we came back to the start in a different direction, then start
    # is actually a corner too.
    corners <- push(corners, start)
  }
  corners
}

solve1 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  corners <- trace_loop(mat)
  loop_len <- 0
  for (i in seq_along(corners)) {
    c1 <- corners[[i]]
    c2 <- corners[[if (i == length(corners)) 1 else i + 1]]
    loop_len <- loop_len + abs(c1[1] - c2[1]) + abs(c1[2] - c2[2])
  }
  cat(loop_len / 2, "\n")
}

solve2 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  corners <- trace_loop(mat)
  enclosed <- 0
  loop_len <- 0
  for (i in seq_along(corners)) {
    c1 <- corners[[i]]
    c2 <- corners[[if (i == length(corners)) 1 else i + 1]]
    # Shoelace formula
    enclosed <- enclosed + c1[1] * c2[2] - c2[1] * c1[2]
    loop_len <- loop_len + abs(c1[1] - c2[1]) + abs(c1[2] - c2[2])
  }
  # Edge cells are counted in the area but they aren't fully enclosed, so we
  # need to subtract them out. Each edge cell contributes 1/2 to the area,
  # except the corners, which contribute 1/4 or 3/4. We have 4 more 90deg
  # corners than 270deg corners, so we add back 1.
  cat(abs(enclosed) / 2 - loop_len / 2 + 1, "\n")
}
