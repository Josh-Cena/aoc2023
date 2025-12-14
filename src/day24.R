library("gmp")

# Solve parametric equation: p1 + t * v1 = p2 + u * v2
# Which is: [[vx1, -vx2], [vy1, -vy2]] @ [t, u]^T = [x2 - x1, y2 - y1]^T
intersection2d <- function(l1, l2) {
  v_mat <- matrix(c(l1[4], l1[5], -l2[4], -l2[5]), nrow = 2)
  b_vec <- c(l2[1] - l1[1], l2[2] - l1[2])
  sol <- tryCatch(solve.bigz(v_mat, b_vec), error = function(e) NULL)
  if (is.null(sol) || any(sol < 0)) {
    return(NULL)
  }
  return(c(l1[1], l1[2]) + sol[1] * c(l1[4], l1[5]))
}

in_range <- function(val, range) {
  low <- range[1]
  high <- range[2]
  return(val >= low && val <= high)
}

cross <- function(v1, v2) {
  return(c(v1[2] * v2[3] - v1[3] * v2[2],
            v1[3] * v2[1] - v1[1] * v2[3],
            v1[1] * v2[2] - v1[2] * v2[1]))
}

solve1 <- function(data) {
  test_area <- as.bigz(strsplit(data[[1]], " ")[[1]])
  hails <- lapply(strsplit(gsub("[@,]", "", data[-1]), " +"), as.bigz)
  total <- 0
  for (i in 1:(length(hails) - 1)) {
    for (j in (i + 1):length(hails)) {
      l1 <- hails[[i]]
      l2 <- hails[[j]]
      sol <- intersection2d(l1, l2)
      if (!is.null(sol) && in_range(sol[1], test_area)
          && in_range(sol[2], test_area)) {
        total <- total + 1
      }
    }
  }
  cat(total, "\n")
}

# Want to solve: p_n + t_n * v_n = p + t_n * v for t_n, v, and p
# ==> (v_n - v) * t_n + p_n = p
# ==> (v_i - v) * t_i + p_i = (v_j - v) * t_j + p_j
# ==> p_j - p_i ∈ span{ v_i - v, v_j - v } = span{ v_i - v, v_j - v_i }
# ==> ((v_j - v_i) × (p_j - p_i)) * (v_i - v) = 0
# ==> ((v_j - v_i) × (p_j - p_i)) * v = ((v_j - v_i) × (p_j - p_i)) * v_i
# ==> [row of A] * v = [cell of b]
# After finding v, plug into p = (v_i - v) * t_i + p_i = (v_j - v) * t_j + p_j
solve2 <- function(data) {
  hails <- lapply(strsplit(gsub("[@,]", "", data[-1]), " +"), as.bigz)
  num_eq <- 3
  A <- matrix(as.bigz(0), nrow = num_eq, ncol = 3)
  b <- matrix(as.bigz(0), nrow = num_eq, ncol = 1)
  for (i in 1:num_eq) {
    j <- i %% num_eq + 1
    v_diff <- hails[[j]][4:6] - hails[[i]][4:6]
    p_diff <- hails[[j]][1:3] - hails[[i]][1:3]
    cross_prod <- cross(v_diff, p_diff)
    A[i, ] <- cross_prod
    b[i] <- sum(cross_prod * hails[[i]][4:6])
  }
  v <- tryCatch(solve.bigz(A, b), error = function(e) NULL)
  if (is.null(v)) {
    cat("No v found\n")
    return()
  }
  p1 <- as.bigz(hails[[1]][1:3])
  p2 <- as.bigz(hails[[2]][1:3])
  v1 <- as.bigz(hails[[1]][4:6])
  v2 <- as.bigz(hails[[2]][4:6])
  pxy <- intersection2d(c(p1, v1 - v), c(p2, v2 - v))
  if (is.null(pxy)) {
    cat("No p found\n")
    return()
  }
  t1 <- (pxy[1] - p1[1]) / (v1[1] - v[1])
  pz <- p1[3] + t1 * (v1[3] - v[3])
  cat(as.character(sum(c(pxy, pz))), "\n")
}
