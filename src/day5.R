map_seed <- function(seed, map) {
  applicable <- map[map$from_start <= seed & map$from_end > seed, ]
  if (nrow(applicable) == 0) {
    # Identity mapping if there are no applicable pieces
    return(seed)
  }
  seed + applicable$diff[1]
}

map_range <- function(range, map) {
  # All pieces that overlap with the range
  applicable <- map[!(map$from_end <= range[1] | map$from_start >= range[2]), ]
  n <- nrow(applicable)
  if (n == 0) {
    # Identity mapping if there are no applicable pieces
    return(list(range))
  }
  # Clip the first and last pieces to the range
  applicable[1, "from_start"] <- max(applicable[1, "from_start"], range[1])
  applicable[n, "from_end"] <- min(applicable[n, "from_end"], range[2])
  ranges <- list()
  last_end <- range[1]
  for (i in seq_len(n)) {
    piece <- applicable[i, ]
    if (piece$from_start > last_end) {
      # Identity mapping if there's no corresponding piece
      ranges <- push(ranges, c(last_end, piece$from_start))
    }
    ranges <- push(ranges, c(piece$from_start, piece$from_end) + piece$diff)
    last_end <- piece$from_end
  }
  if (last_end < range[2]) {
    ranges <- push(ranges, c(last_end, range[2]))
  }
  ranges
}

parse_input <- function(data) {
  seeds <- data[1]
  seeds <- substr(seeds, nchar("seeds: ") + 1, nchar(seeds))
  seeds <- as.numeric(strsplit(seeds, " ")[[1]])
  maps <- lapply(
    strsplit(paste(data[3:length(data)], collapse = "\n"), "\n\n")[[1]],
    function(x) {
      lines <- strsplit(x, "\n")[[1]]
      # Columns: to, from, range_len
      map <- matrix(
        sapply(strsplit(lines[2:length(lines)], " "), as.numeric),
        ncol = 3,
        byrow = TRUE
      )
      # New columns: from_start, from_end, diff
      map[, 1] <- map[, 1] - map[, 2]
      map[, 3] <- map[, 2] + map[, 3]
      map[, c(1, 2, 3)] <- map[, c(2, 3, 1)]
      map <- map[order(map[, 1]), ]
      setNames(data.frame(map), c("from_start", "from_end", "diff"))
    }
  )
  list(seeds = seeds, maps = maps)
}

solve1 <- function(data) {
  parsed <- parse_input(data)
  seeds <- parsed$seeds
  maps <- parsed$maps
  for (map in maps) {
    seeds <- sapply(seeds, map_seed, map = map)
  }
  cat(min(seeds), "\n")
}

solve2 <- function(data) {
  parsed <- parse_input(data)
  seeds <- parsed$seeds
  maps <- parsed$maps
  ranges <- matrix(seeds, ncol = 2, byrow = TRUE)
  ind <- seq_len(nrow(ranges))
  ranges[ind, 2] <- ranges[ind, 1] + ranges[ind, 2]
  ranges <- lapply(seq_len(nrow(ranges)), function(i) ranges[i, ])
  for (map in maps) {
    ranges <- do.call(c, lapply(ranges, map_range, map = map))
  }
  cat(min(sapply(ranges, function(x) x[1])), "\n")
}
