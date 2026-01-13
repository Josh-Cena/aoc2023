library("collections", include.only = "priority_queue")

dir_to_diff <- list(c(0, 1), c(1, 0), c(0, -1), c(-1, 0))

neighbors <- function(w, h, node) {
  res <- list()
  for (ndir in 1:4) {
    if (node[3] == ndir && node[4] == 3) {
      next
    }
    if (
      node[3] == 1 &&
        ndir == 3 ||
        node[3] == 3 && ndir == 1 ||
        node[3] == 2 && ndir == 4 ||
        node[3] == 4 && ndir == 2
    ) {
      # Can't turn 180 degrees
      next
    }
    diff <- dir_to_diff[[ndir]]
    ni <- node[1] + diff[1]
    nj <- node[2] + diff[2]
    if (ni >= 1 && ni <= h && nj >= 1 && nj <= w) {
      nsteps <- if (node[3] == ndir) node[4] + 1 else 1
      res <- push(res, c(ni, nj, ndir, nsteps))
    }
  }
  res
}

neighbors_ultra <- function(w, h, node) {
  res <- list()
  dirs <- if (node[4] < 4) node[3] else 1:4
  for (ndir in dirs) {
    if (node[3] == ndir && node[4] == 10) {
      next
    }
    if (
      node[3] == 1 &&
        ndir == 3 ||
        node[3] == 3 && ndir == 1 ||
        node[3] == 2 && ndir == 4 ||
        node[3] == 4 && ndir == 2
    ) {
      # Can't turn 180 degrees
      next
    }
    diff <- dir_to_diff[[ndir]]
    ni <- node[1] + diff[1]
    nj <- node[2] + diff[2]
    if (ni >= 1 && ni <= h && nj >= 1 && nj <= w) {
      nsteps <- if (node[3] == ndir) node[4] + 1 else 1
      res <- push(res, c(ni, nj, ndir, nsteps))
    }
  }
  res
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
  pq$push(c(1, 2, 1, 1, mat[[1, 2]]), priority = -mat[[1, 2]])
  pq$push(c(2, 1, 2, 1, mat[[2, 1]]), priority = -mat[[2, 1]])
  while (pq$size() > 0) {
    u <- pq$pop()
    if (u[1] == height && u[2] == width) {
      return(u[5])
    }
    for (v in neighbors_cb(width, height, u)) {
      new_cost <- u[5] + mat[v[1], v[2]]
      if (new_cost < dist[v[1], v[2], v[3], v[4]]) {
        dist[v[1], v[2], v[3], v[4]] <- new_cost
        pq$push(c(v[1], v[2], v[3], v[4], new_cost), priority = -new_cost)
      }
    }
  }
  stop("No path found")
}

solve1 <- function(data) {
  mat <- as.matrix(sapply(strsplit(data, ""), unlist))
  mat <- apply(mat, 1, as.numeric)
  dist <- dijkstra(mat, neighbors, 3)
  cat(dist, "\n")
}

solve2 <- function(data) {
  mat <- as.matrix(sapply(strsplit(data, ""), unlist))
  mat <- apply(mat, 1, as.numeric)
  dist <- dijkstra(mat, neighbors_ultra, 10)
  cat(dist, "\n")
}
