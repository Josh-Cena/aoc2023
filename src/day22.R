library("collections", include.only = "queue")

x_y_intersect <- function(block1, block2) {
  x_intersect <-
    max(block1$start[1], block2$start[1]) <= min(block1$end[1], block2$end[1])
  y_intersect <-
    max(block1$start[2], block2$start[2]) <= min(block1$end[2], block2$end[2])
  x_intersect && y_intersect
}

create_static_stack <- function(data) {
  blocks <- lapply(data, function(line) {
    parts <- strsplit(line, "~")[[1]]
    start <- as.numeric(unlist(strsplit(parts[1], ",")))
    end <- as.numeric(unlist(strsplit(parts[2], ",")))
    list(
      start = start,
      end = end,
      supported_by = integer(0),
      supports = integer(0)
    )
  })
  blocks <- blocks[order(sapply(blocks, function(x) x$start[3]))]
  order_by_top <- order(sapply(blocks, function(x) x$end[3]))
  for (i in 2:length(blocks)) {
    eventual_z <- -1
    for (j in rev(order_by_top)) {
      bj <- blocks[[j]]
      if (blocks[[j]]$end[3] < eventual_z) {
        break
      }
      if (blocks[[i]]$start[3] > bj$end[3] && x_y_intersect(blocks[[i]], bj)) {
        if (eventual_z == -1) {
          eventual_z <- bj$end[3]
          diff <- blocks[[i]]$start[3] - eventual_z - 1
          blocks[[i]]$start[3] <- blocks[[i]]$start[3] - diff
          blocks[[i]]$end[3] <- blocks[[i]]$end[3] - diff
          # Yeah... very inefficient
          order_by_top <- order(sapply(blocks, function(x) x$end[3]))
        }
        blocks[[i]]$supported_by <- c(blocks[[i]]$supported_by, j)
        blocks[[j]]$supports <- c(bj$supports, i)
      }
    }
    if (eventual_z == -1) {
      eventual_z <- 0
      diff <- blocks[[i]]$start[3] - eventual_z - 1
      blocks[[i]]$start[3] <- blocks[[i]]$start[3] - diff
      blocks[[i]]$end[3] <- blocks[[i]]$end[3] - diff
      order_by_top <- order(sapply(blocks, function(x) x$end[3]))
    }
  }
  return(blocks)
}

solve1 <- function(data) {
  blocks <- create_static_stack(data)
  total <- sum(sapply(blocks, function(x) {
    all(sapply(blocks[x$supports], function(x) length(x$supported_by) > 1))
  }))
  cat(total, "\n")
}

num_fall <- function(blocks, gone) {
  q <- queue()
  q$push(gone)
  fall <- rep(FALSE, length(blocks))
  fall[gone] <- TRUE
  while (q$size() > 0) {
    i <- q$pop()
    for (j in blocks[[i]]$supports) {
      if (all(fall[blocks[[j]]$supported_by])) {
        fall[j] <- TRUE
        q$push(j)
      }
    }
  }
  return(sum(fall) - 1)
}

solve2 <- function(data) {
  blocks <- create_static_stack(data)
  total <- 0
  for (i in seq_along(blocks)) {
    if (length(blocks[[i]]$supports) > 0) {
      total <- total + num_fall(blocks, i)
    }
  }
  cat(total, "\n")
}
