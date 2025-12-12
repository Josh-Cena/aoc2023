remove_seg <- function(segs, start, end) {
  # Remove fully contained segments
  new_segs <- segs[!(segs$starts >= start & segs$ends <= end), ]
  if (nrow(new_segs) == 0) {
    return(new_segs)
  }
  removed_rows <- NULL
  added_rows <- NULL
  # Adjust partial overlap
  for (i in 1:nrow(new_segs)) {
    # If there's a single segment that fully contains [start, end),
    # it needs to be split into two segments.
    if (new_segs$starts[i] < start && new_segs$ends[i] > end) {
      removed_rows <- c(removed_rows, i)
      added_rows <- rbind(
        added_rows,
        data.frame(
          starts = new_segs$starts[i],
          ends = start,
          data = new_segs$data[i]
        ),
        data.frame(
          starts = end,
          ends = new_segs$ends[i],
          data = new_segs$data[i]
        )
      )
    } else if (new_segs$starts[i] < start && new_segs$ends[i] > start) {
      # If the segment contains start, adjust its end
      new_segs$ends[i] <- start
      if (new_segs$starts[i] == new_segs$ends[i]) {
        removed_rows <- c(removed_rows, i)
      }
    } else if (new_segs$starts[i] < end && new_segs$ends[i] > end) {
      # If the segment contains end, adjust its start
      new_segs$starts[i] <- end
      if (new_segs$starts[i] == new_segs$ends[i]) {
        removed_rows <- c(removed_rows, i)
      }
    }
  }
  if (!is.null(removed_rows)) new_segs <- new_segs[-removed_rows, ]
  if (!is.null(added_rows)) new_segs <- rbind(new_segs, added_rows)
  return(new_segs)
}

compose_maps <- function(map1, map2) {
  res <- list()
  map1_segs <- setNames(data.frame(map1), c("starts", "ends", "data"))
  map2_segs <- setNames(data.frame(map2), c("starts", "ends", "data"))

  for (i in 1:nrow(map1)) {
    si <- map1$from_start[i]
    ei <- map1$from_end[i]
    di <- map1$diff[i]

    # For each map1 piece: find all map2 pieces that overlap
    map2_rev <- map2
    map2_rev$from_start <- map2$from_start - di
    map2_rev$from_end <- map2$from_end - di
    for (j in 1:nrow(map2_rev)) {
      s <- max(si, map2_rev$from_start[j])
      e <- min(ei, map2_rev$from_end[j])

      if (s < e) {
        # [s, e) |-> [s + di, e + di) |-> [s + di + dj, e + di + dj)
        res[[length(res) + 1]] <- data.frame(
          from_start = s,
          from_end = e,
          diff = di + map2$diff[j]
        )
        # Remove this composite range from map1_segs and map2_segs
        map1_segs <- remove_seg(map1_segs, s, e)
        map2_segs <- remove_seg(map2_segs, s + di, e + di)
      }
    }
  }
  # Add the remaining parts of map1
  if (length(map1_segs$starts) > 0) {
    res[[length(res) + 1]] <- data.frame(
      from_start = map1_segs$starts,
      from_end = map1_segs$ends,
      diff = map1_segs$data
    )
  }
  if (length(map2_segs$starts) > 0) {
    res[[length(res) + 1]] <- data.frame(
      from_start = map2_segs$starts,
      from_end = map2_segs$ends,
      diff = map2_segs$data
    )
  }

  res <- do.call(rbind, res)
  res <- res[order(res$from_start), ]
  res
}

map_seed <- function(seed, map) {
  applicable <- map[map$from_start <= seed & map$from_end > seed, ]
  if (nrow(applicable) > 0) {
    return(seed + applicable$diff[1])
  }
  return(seed)
}

map_range_min <- function(range, map) {
  # All segments that overlap with the range
  applicable <- map[!(map$from_end <= range[1] | map$from_start >= range[2]), ]
  range_min <- map_seed(range[1], map)
  if (nrow(applicable) > 0) {
    for (i in 1:nrow(applicable)) {
      s <- max(range[1], applicable$from_start[i])
      range_min <- min(range_min, s + applicable$diff[i])
    }
  }
  return(range_min)
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
        ncol = 3, byrow = TRUE
      )
      # New columns: from_start, from_end, diff
      map[, 1] <- map[, 1] - map[, 2]
      map[, 3] <- map[, 2] + map[, 3]
      map[, c(1, 2, 3)] <- map[, c(2, 3, 1)]
      map <- setNames(data.frame(map), c("from_start", "from_end", "diff"))
      return(map)
    }
  )
  return(list(seeds = seeds, maps = maps))
}

solve1 <- function(data) {
  parsed <- parse_input(data)
  seeds <- parsed$seeds
  maps <- parsed$maps
  all_map <- maps[[1]]
  for (map in maps[2:length(maps)]) {
    all_map <- compose_maps(all_map, map)
  }
  mapped_seeds <- sapply(seeds, map_seed, map = all_map)
  cat(min(mapped_seeds), "\n")
}

solve2 <- function(data) {
  parsed <- parse_input(data)
  seeds <- parsed$seeds
  maps <- parsed$maps
  seeds <- matrix(seeds, ncol = 2, byrow = TRUE)
  seeds[1:nrow(seeds), 2] <- seeds[1:nrow(seeds), 1] + seeds[1:nrow(seeds), 2]
  all_map <- maps[[1]]
  for (map in maps[2:length(maps)]) {
    all_map <- compose_maps(all_map, map)
  }
  mapped_ranges <- apply(seeds, 1, map_range_min, map = all_map)
  cat(min(mapped_ranges), "\n")
}
