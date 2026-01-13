library("gmp", include.only = "as.bigz")
library("collections", include.only = "queue")

bfs <- function(mat, start) {
  reachable <- matrix(FALSE, nrow(mat), ncol(mat))
  q <- queue()
  q$push(start)
  while (q$size() > 0) {
    pos <- q$pop()
    for (dir in list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))) {
      next_pos <- pos + dir
      nx <- next_pos[1]
      ny <- next_pos[2]
      if (
        nx >= 1 &&
          nx <= nrow(mat) &&
          ny >= 1 &&
          ny <= ncol(mat) &&
          !reachable[nx, ny] &&
          mat[nx, ny] != "#"
      ) {
        reachable[nx, ny] <- TRUE
        q$push(next_pos)
      }
    }
  }
  reachable
}

# Picture all cells we can reach within T steps as a diamond of radius T.
# This diamond's region contains two kinds of cells: reachable in odd steps,
# and in even steps.
# Every time we expand the diamond, we include new odd cells and even cells,
# which are exactly those along the diamond's boundary and are not rocks.
# Odd steps can introduce odd cells, and even steps can introduce even cells.
# Therefore, all cells reachable with 64 steps are those that:
# 1. are not rocks,
# 2. are in the diamond with radius 64,
# 3. have even parity
solve1 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  start <- which(mat == "S", arr.ind = TRUE)
  mat[start] <- "."
  dist <- abs(row(mat) - start[1]) + abs(col(mat) - start[2])
  # Sadly, we actually have to run BFS instead of doing mat == "."
  # There are some holes in the field that are fully surrounded by rocks,
  # and not actually reachable.
  reachable <- bfs(mat, start)
  res <- sum(reachable & (dist <= 64) & (dist %% 2 == 0))
  cat(res, "\n")
}

# We now have a grid of matrices. Because each matrix's width is odd, the
# *matrices* themselves also switch parity in a chessboard fashion (that is, an
# odd cell in this matrix would become an even cell in the edge-adjacent
# matrix).
# The starting matrix is considered "even" (it has distance 0 to itself).
# For any diamond of radius T on this chessboard, to get all odd cells in it,
# we need to count:
# - #more than half-covered odd matrix * #even cells in a matrix
# - #more than half-covered even matrix * #odd cells in a matrix
# - Add/subtract odd cells in less than half-covered even matrices
# - Add/subtract even cells in less than half-covered odd matrices
# The last two terms are very, very tricky, so I give up on computing them in
# full generality (especially because when the corner of the diamond just passed
# the matrix's center--neither the included part nor than the excluded part is
# a triangle!). However, the problem is too nicely structured:
# 26501365 = 65 + 131 * 202300, meaning we just travel 202300 matrices after
# reaching the edge of the starting matrix.
# This means that the final diamond actually touches the four edges. Here's
# one such corner of the diamond, where 0 represents an even matrix and 1
# represents an odd matrix, using matrix width = 5 for illustration (X itself
# is included). "+" represents corners to be added, "-" represents corners to
# be subtracted:
#
# 1 1 1 1 1
# 1 1 1 1 +
# 1 1 1 + +
# - - X 0 0
# - X 0 0 0
# X 0 0 0 0
# - X 0 0 0
# - - X 0 0
#
# In this configuration, if we traveled R more matrices after reaching the edge
# of the starting matrix, we would have (R+1)^2 even and R^2 odd matrices that
# we cover more than half of (the "0" matrix above included, "1" not included).
# Then, we need to remove some odd cells in even matrix corners and add some
# odd cells in odd matrix corners (equivalently, even cells in even matrices).
# Each 4 of these even corners correspond to the 4 corners of a matrix, and so
# do each 4 of the odd corners. We can count the number of odd cells in the
# starting matrix's corners (to remove), as well as the number of even cells
# in the starting matrix's corners to add:
#
# * * . * *
# * . . . *
# . . . . .
# * . . . *
# * * . * *
solve2 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  w <- nrow(mat)
  start <- which(mat == "S", arr.ind = TRUE)
  mat[start] <- "."
  dist <- abs(row(mat) - start[1]) + abs(col(mat) - start[2])
  is_even <- dist %% 2 == 0
  is_corner <- dist >= start[1]
  reachable <- bfs(mat, start)
  even_cells_in_mat <- as.bigz(sum(is_even & reachable))
  odd_cells_in_mat <- as.bigz(sum(!is_even & reachable))
  even_corners_in_mat <- as.bigz(sum(is_corner & is_even & reachable))
  odd_corners_in_mat <- as.bigz(sum(is_corner & !is_even & reachable))
  mat_radius <- as.bigz((26501365 - (w - 1) / 2) / w)
  total <- odd_cells_in_mat *
    (mat_radius + 1)^2 +
    even_cells_in_mat * mat_radius^2 +
    even_corners_in_mat * mat_radius -
    odd_corners_in_mat * (mat_radius + 1)
  cat(as.character(total), "\n")
}
