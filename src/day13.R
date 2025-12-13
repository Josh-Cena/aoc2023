# axis = 2 means the axis is between second & third row/column
is_symmetric_about <- function(data, axis, dir, tolerance) {
  n <- if (dir == "row") nrow(data) else ncol(data)
  range_half <- axis:max(1, 2 * axis + 1 - n)
  range_other <- (axis + 1):min(n, 2 * axis)
  if (dir == "row") {
    reflected_half <- data[range_half, , drop = FALSE]
    other <- data[range_other, , drop = FALSE]
  } else {
    reflected_half <- data[, range_half, drop = FALSE]
    other <- data[, range_other, drop = FALSE]
  }
  difference <- sum(other != reflected_half)
  return(difference == tolerance)
}

num_axes <- function(data, tolerance) {
  total <- 0
  for (i in 1:(nrow(data) - 1)) {
    if (is_symmetric_about(data, i, "row", tolerance)) {
      total <- total + 100 * i
    }
  }
  for (i in 1:(ncol(data) - 1)) {
    if (is_symmetric_about(data, i, "col", tolerance)) {
      total <- total + i
    }
  }
  return(total)
}

solve1 <- function(data) {
  data <- strsplit(strsplit(paste(data, collapse = "\n"), "\n\n")[[1]], "\n")
  data <- lapply(data, function(x) {
    matrix(unlist(strsplit(x, "")), nrow = length(x), byrow = TRUE)
  })
  cat(sum(sapply(data, num_axes, tolerance = 0)), "\n")
}

solve2 <- function(data) {
  data <- strsplit(strsplit(paste(data, collapse = "\n"), "\n\n")[[1]], "\n")
  data <- lapply(data, function(x) {
    matrix(unlist(strsplit(x, "")), nrow = length(x), byrow = TRUE)
  })
  cat(sum(sapply(data, num_axes, tolerance = 1)), "\n")
}
