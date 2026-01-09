library("collections", include.only = "priority_queue")

dir_to_diff <- list(
  c(0, 1),
  c(1, 0),
  c(0, -1),
  c(-1, 0)
)

neighbors <- function(w, h, node) {
  res <- list()
  for (ndir in 1:4) {
    if (node$dir == ndir && node$steps == 3) {
      next
    }
    if (
      node$dir == 1 &&
        ndir == 3 ||
        node$dir == 3 && ndir == 1 ||
        node$dir == 2 && ndir == 4 ||
        node$dir == 4 && ndir == 2
    ) {
      # Can't turn 180 degrees
      next
    }
    diff <- dir_to_diff[[ndir]]
    ni <- node$i + diff[1]
    nj <- node$j + diff[2]
    if (ni >= 1 && ni <= h && nj >= 1 && nj <= w) {
      nsteps <- if (node$dir == ndir) node$steps + 1 else 1
      res[[length(res) + 1]] <- list(i = ni, j = nj, dir = ndir, steps = nsteps)
    }
  }
  return(res)
}

neighbors_ultra <- function(w, h, node) {
  res <- list()
  if (node$steps < 4) {
    dirs <- node$dir
  } else {
    dirs <- 1:4
  }
  for (ndir in dirs) {
    if (node$dir == ndir && node$steps == 10) {
      next
    }
    if (
      node$dir == 1 &&
        ndir == 3 ||
        node$dir == 3 && ndir == 1 ||
        node$dir == 2 && ndir == 4 ||
        node$dir == 4 && ndir == 2
    ) {
      # Can't turn 180 degrees
      next
    }
    diff <- dir_to_diff[[ndir]]
    ni <- node$i + diff[1]
    nj <- node$j + diff[2]
    if (ni >= 1 && ni <= h && nj >= 1 && nj <= w) {
      nsteps <- if (node$dir == ndir) node$steps + 1 else 1
      res[[length(res) + 1]] <- list(i = ni, j = nj, dir = ndir, steps = nsteps)
    }
  }
  return(res)
}

dijkstra <- function(mat, neighbors_cb, max_steps) {
  width <- ncol(mat)
  height <- nrow(mat)
  # Graph: position, direction, number of steps in that direction
  # 1 = right, 2 = down, 3 = left, 4 = up
  dist <- array(Inf, dim = c(height, width, 4, max_steps))
  dist[1, 2, 1, 1] <- mat[[1, 2]]
  dist[2, 1, 2, 1] <- mat[[2, 1]]
  # Max-queue, so all priorities are negative
  pq <- priority_queue()
  pq$push(
    list(i = 1, j = 2, dir = 1, steps = 1, cost = mat[[1, 2]]),
    priority = -mat[[1, 2]]
  )
  pq$push(
    list(i = 2, j = 1, dir = 2, steps = 1, cost = mat[[2, 1]]),
    priority = -mat[[2, 1]]
  )
  while (pq$size() > 0) {
    u <- pq$pop()
    for (v in neighbors_cb(width, height, u)) {
      new_cost <- u$cost + mat[v$i, v$j]
      if (new_cost < dist[v$i, v$j, v$dir, v$steps]) {
        dist[v$i, v$j, v$dir, v$steps] <- new_cost
        pq$push(
          list(i = v$i, j = v$j, dir = v$dir, steps = v$steps, cost = new_cost),
          priority = -new_cost
        )
      }
    }
  }
  dist
}

solve1 <- function(data) {
  mat <- as.matrix(sapply(strsplit(data, ""), unlist))
  mat <- apply(mat, 1, as.numeric)
  dist <- dijkstra(mat, neighbors, 3)
  width <- ncol(mat)
  height <- nrow(mat)
  cat(min(dist[height, width, , ]), "\n")
}

solve2 <- function(data) {
  mat <- as.matrix(sapply(strsplit(data, ""), unlist))
  mat <- apply(mat, 1, as.numeric)
  dist <- dijkstra(mat, neighbors_ultra, 10)
  width <- ncol(mat)
  height <- nrow(mat)
  cat(min(dist[height, width, , ]), "\n")
}
