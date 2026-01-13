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
  order_by_top <- order(
    sapply(blocks[1], function(x) x$end[3]),
    decreasing = TRUE
  )
  for (i in 2:length(blocks)) {
    eventual_z <- -1
    for (j in order_by_top) {
      bi <- blocks[[i]]
      bj <- blocks[[j]]
      if (bj$end[3] < eventual_z) {
        break
      }
      if (bi$start[3] <= bj$end[3] || !x_y_intersect(bi, bj)) {
        next
      }
      if (eventual_z == -1) {
        eventual_z <- bj$end[3]
        diff <- bi$start[3] - eventual_z - 1
        blocks[[i]]$start[3] <- bi$start[3] - diff
        blocks[[i]]$end[3] <- bi$end[3] - diff
        # Yeah... very inefficient
        order_by_top <- order(
          sapply(blocks[1:i], function(x) x$end[3]),
          decreasing = TRUE
        )
      }
      blocks[[i]]$supported_by <- c(bi$supported_by, j)
      blocks[[j]]$supports <- c(bj$supports, i)
    }
    if (eventual_z == -1) {
      eventual_z <- 0
      diff <- blocks[[i]]$start[3] - eventual_z - 1
      blocks[[i]]$start[3] <- blocks[[i]]$start[3] - diff
      blocks[[i]]$end[3] <- blocks[[i]]$end[3] - diff
      order_by_top <- order(
        sapply(blocks[1:i], function(x) x$end[3]),
        decreasing = TRUE
      )
    }
  }
  blocks
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
  sum(fall) - 1
}

solve2 <- function(data) {
  blocks <- create_static_stack(data)
  total <- sum(sapply(seq_along(blocks), function(i) num_fall(blocks, i)))
  cat(total, "\n")
}
