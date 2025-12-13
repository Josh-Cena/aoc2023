parse_line <- function(line) {
  line <- strsplit(line, ": ")[[1]][2]
  line <- strsplit(line, "; ")[[1]]
  line <- sapply(line, function(x) {
    x <- strsplit(x, ", ")[[1]]
    counts <- list("red" = 0, "blue" = 0, "green" = 0)
    for (item in x) {
      parts <- strsplit(item, " ")[[1]]
      color <- parts[2]
      count <- as.numeric(parts[1])
      counts[[color]] <- counts[[color]] + count
    }
    return(counts)
  })
  return(line)
}

solve1 <- function(data) {
  lines <- sapply(data, parse_line)
  total <- 0
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    if (all(line["red", ] <= 12
        & line["green", ] <= 13
        & line["blue", ] <= 14)) {
      total <- total + i
    }
  }
  cat(total, "\n")
}

solve2 <- function(data) {
  lines <- sapply(data, parse_line)
  powers <- sapply(lines, function(line) {
    return(max(unlist(line["red", ]))
      * max(unlist(line["green", ]))
      * max(unlist(line["blue", ])))
  })
  cat(sum(powers), "\n")
}
