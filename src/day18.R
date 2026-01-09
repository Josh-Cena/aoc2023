library("gmp", include.only = "as.bigz")

dir_to_diff <- list(
  "R" = c(0, 1),
  "L" = c(0, -1),
  "U" = c(-1, 0),
  "D" = c(1, 0)
)

solve <- function(dirs, dists) {
  cur_r <- 1
  cur_c <- 1
  all_r <- 1
  all_c <- 1
  for (i in seq_len(length(dirs))) {
    dir <- dirs[i]
    dist <- dists[i]
    dr <- dir_to_diff[[dir]][1] * dist
    dc <- dir_to_diff[[dir]][2] * dist
    cur_r <- cur_r + dr
    cur_c <- cur_c + dc
    all_r <- c(all_r, cur_r)
    all_c <- c(all_c, cur_c)
  }
  all_r <- sort(unique(all_r))
  all_c <- sort(unique(all_c))
  min_r <- all_r[1]
  min_c <- all_c[1]
  max_r <- all_r[length(all_r)]
  max_c <- all_c[length(all_c)]
  # Double compressed coords, so that we can capture holes & slits
  r_compressor <- setNames(seq_along(all_r) * 2, as.character(all_r))
  c_compressor <- setNames(seq_along(all_c) * 2, as.character(all_c))
  r_decompressor <- rep(1, length(all_r) * 2 + 1)
  c_decompressor <- rep(1, length(all_c) * 2 + 1)
  r_decompressor[1] <- min_r - 1
  r_decompressor[seq_along(all_r) * 2] <- all_r
  r_decompressor[seq_along(all_r) * 2 + 1] <- all_r + 1
  c_decompressor[1] <- min_c - 1
  c_decompressor[seq_along(all_c) * 2] <- all_c
  c_decompressor[seq_along(all_c) * 2 + 1] <- all_c + 1
  # Each cell's index corresponds to its top-left corner's coordinates.
  # col 1 -> min_c - 1,
  # col 2 -> min_c, col 3 -> min_c + 1,
  # col 4 -> all_c[2], col 5 -> all_c[2] + 1, ...
  # col n - 1 -> max_c, col n -> max_c + 1
  mat <- matrix(
    FALSE,
    nrow = length(all_r) * 2 + 1,
    ncol = length(all_c) * 2 + 1
  )
  cur_r <- 1
  cur_c <- 1
  for (i in seq_along(dirs)) {
    dir <- dirs[i]
    dist <- dists[i]
    dr <- dir_to_diff[[dir]][1] * dist
    dc <- dir_to_diff[[dir]][2] * dist
    mat_r <- r_compressor[as.character(cur_r)]
    mat_c <- c_compressor[as.character(cur_c)]
    mat_r_2 <- r_compressor[as.character(cur_r + dr)]
    mat_c_2 <- c_compressor[as.character(cur_c + dc)]
    mat[mat_r:mat_r_2, mat_c:mat_c_2] <- TRUE
    cur_r <- cur_r + dr
    cur_c <- cur_c + dc
  }
  outside <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  stack <- list(c(1, 1))
  visited <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  while (length(stack) > 0) {
    pos <- stack[[1]]
    stack <- stack[-1]
    for (dir in c("R", "L", "U", "D")) {
      next_pos <- pos + dir_to_diff[[dir]]
      r <- next_pos[1]
      c <- next_pos[2]
      if (
        r >= 1 &&
          r <= nrow(mat) &&
          c >= 1 &&
          c <= ncol(mat) &&
          !mat[r, c] &&
          !visited[r, c]
      ) {
        # We need the area represented by this cell.
        # Each cell's index corresponds to its top-left corner's coordinates.
        r_decompressed <- r_decompressor[[r]]
        c_decompressed <- c_decompressor[[c]]
        next_r <- if (r == nrow(mat)) max_r + 2 else r_decompressor[[r + 1]]
        next_c <- if (c == ncol(mat)) max_c + 2 else c_decompressor[[c + 1]]
        area <- (next_r - r_decompressed) * (next_c - c_decompressed)
        outside[r, c] <- area
        visited[r, c] <- TRUE
        stack <- c(stack, list(next_pos))
      }
    }
  }
  total <- as.bigz(max_r - min_r + 3) * as.bigz(max_c - min_c + 3)
  outside <- as.bigz(outside)
  cat(as.character(total - sum(outside)), "\n")
}

solve1 <- function(data) {
  parts <- strsplit(data, " ")
  dirs <- sapply(parts, function(x) x[1])
  dists <- sapply(parts, function(x) as.numeric(x[2]))
  solve(dirs, dists)
}

solve2 <- function(data) {
  parts <- strsplit(data, " ")
  dirs <- as.numeric(
    sapply(parts, function(x) substr(x[3], nchar(x[3]) - 1, nchar(x[3]) - 1))
  )
  dirs <- c("R", "D", "L", "U")[dirs + 1]
  dists <- sapply(parts, function(x) substr(x[3], 3, nchar(x[3]) - 2))
  dists <- strtoi(dists, base = 16)
  solve(dirs, dists)
}
