deflect_to <- list(
  "." = list(left = "left", right = "right", up = "up", down = "down"),
  "/" = list(left = "down", right = "up", up = "right", down = "left"),
  "\\" = list(left = "up", right = "down", up = "left", down = "right"),
  "-" = list(left = "left", right = "right",
          up = c("left", "right"), down = c("left", "right")),
  "|" = list(left = c("up", "down"), right = c("up", "down"),
          up = "up", down = "down")
)

# Remember that these are not ray directions, but the side of the cell
# that the light is entering.
dir_to_index <- list(
  "left" = c(0, 1),
  "right" = c(0, -1),
  "up" = c(1, 0),
  "down" = c(-1, 0)
)

energize <- function(mat, r_init, c_init, dir_init) {
  # Which direction have we seen light coming into this cell?
  # These are *entering* sides, not light direction (which is opposite)
  has_up <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  has_down <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  has_left <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  has_right <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  has_left[[r_init, c_init]] <- TRUE
  stack <- list()
  stack[[1]] <- list(r = r_init, c = c_init, dir = dir_init)
  while (length(stack) > 0) {
    cur <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    new_dirs <- deflect_to[[mat[[cur$r, cur$c]]]][[cur$dir]]
    for (dir in new_dirs) {
      d <- dir_to_index[[dir]]
      new_r <- cur$r + d[1]
      new_c <- cur$c + d[2]
      if (new_r < 1 || new_r > nrow(mat) || new_c < 1 || new_c > ncol(mat)) {
        next
      }
      if (dir == "left" && !has_left[[new_r, new_c]]) {
        has_left[[new_r, new_c]] <- TRUE
        stack[[length(stack) + 1]] <- list(r = new_r, c = new_c, dir = dir)
      } else if (dir == "right" && !has_right[[new_r, new_c]]) {
        has_right[[new_r, new_c]] <- TRUE
        stack[[length(stack) + 1]] <- list(r = new_r, c = new_c, dir = dir)
      } else if (dir == "up" && !has_up[[new_r, new_c]]) {
        has_up[[new_r, new_c]] <- TRUE
        stack[[length(stack) + 1]] <- list(r = new_r, c = new_c, dir = dir)
      } else if (dir == "down" && !has_down[[new_r, new_c]]) {
        has_down[[new_r, new_c]] <- TRUE
        stack[[length(stack) + 1]] <- list(r = new_r, c = new_c, dir = dir)
      }
    }
  }
  sum(has_up | has_down | has_left | has_right)
}

solve1 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  energized <- energize(mat, 1, 1, "left")
  cat(energized, "\n")
}

solve2 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  max_energized <- 0
  # I know this isn't the most efficient because at some point we can memoize
  # and use an existing calculation, but we just have 400 edge cells anyway,
  # and again memoization is a PITA in R due to the lack of hashing.
  for (r in 1:nrow(mat)) {
    energized <- energize(mat, r, 1, "left")
    if (energized > max_energized) {
      max_energized <- energized
    }
    energized <- energize(mat, r, ncol(mat), "right")
    if (energized > max_energized) {
      max_energized <- energized
    }
  }
  for (c in 1:ncol(mat)) {
    energized <- energize(mat, 1, c, "up")
    if (energized > max_energized) {
      max_energized <- energized
    }
    energized <- energize(mat, nrow(mat), c, "down")
    if (energized > max_energized) {
      max_energized <- energized
    }
  }
  cat(max_energized, "\n")
}
