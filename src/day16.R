library("collections", include.only = "stack", warn.conflicts = FALSE)

# left = 1, right = 2, up = 3, down = 4
deflect_to <- list(
  "." = list(1, 2, 3, 4),
  "/" = list(4, 3, 2, 1),
  "\\" = list(3, 4, 1, 2),
  "-" = list(1, 2, c(1, 2), c(1, 2)),
  "|" = list(c(3, 4), c(3, 4), 3, 4)
)

dir_to_diff <- list(c(0, -1), c(0, 1), c(-1, 0), c(1, 0))

energize <- function(mat, r_init, c_init, dir_init) {
  # Do we have a light in this cell going in this direction?
  has_light <- lapply(1:4, function(.) matrix(FALSE, nrow(mat), ncol(mat)))
  has_light[[dir_init]][[r_init, c_init]] <- TRUE
  stk <- stack()
  stk$push(list(r = r_init, c = c_init, dir = dir_init))
  while (stk$size() > 0) {
    cur <- stk$pop()
    new_dirs <- deflect_to[[mat[[cur$r, cur$c]]]][[cur$dir]]
    for (dir in new_dirs) {
      d <- dir_to_diff[[dir]]
      new_r <- cur$r + d[1]
      new_c <- cur$c + d[2]
      if (new_r < 1 || new_r > nrow(mat) || new_c < 1 || new_c > ncol(mat)) {
        next
      }
      if (!has_light[[dir]][[new_r, new_c]]) {
        has_light[[dir]][[new_r, new_c]] <- TRUE
        stk$push(list(r = new_r, c = new_c, dir = dir))
      }
    }
  }
  sum(has_light[[1]] | has_light[[2]] | has_light[[3]] | has_light[[4]])
}

solve1 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  # Initial light goes to the right
  energized <- energize(mat, 1, 1, 2)
  cat(energized, "\n")
}

solve2 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  max_energized <- 0
  # I know this isn't the most efficient because at some point we can memoize
  # and use an existing calculation, but we just have 400 edge cells anyway,
  # and again memoization is a PITA in R due to the lack of hashing.
  for (r in seq_len(nrow(mat))) {
    max_energized <- max(max_energized, energize(mat, r, 1, 2))
    max_energized <- max(max_energized, energize(mat, r, ncol(mat), 1))
  }
  for (c in seq_len(ncol(mat))) {
    max_energized <- max(max_energized, energize(mat, 1, c, 4))
    max_energized <- max(max_energized, energize(mat, nrow(mat), c, 3))
  }
  cat(max_energized, "\n")
}
