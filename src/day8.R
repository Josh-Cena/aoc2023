library("gmp", include.only = c("lcm.bigz"))

solve1 <- function(data) {
  instructions <- strsplit(data[1], "")[[1]]
  map <- lapply(data[3:length(data)], function(l) {
    parts <- strsplit(l, " = ")[[1]]
    targets <- strsplit(substr(parts[2], 2, nchar(parts[2]) - 1), ", ")[[1]]
    list(source = parts[1], targets = targets)
  })
  map <- setNames(lapply(map, `[[`, "targets"), lapply(map, `[[`, "source"))
  map <- t(data.frame(map))
  count <- 0
  cur <- "AAA"
  while (cur != "ZZZ") {
    if (instructions[count %% length(instructions) + 1] == "L") {
      cur <- map[cur, 1]
    } else {
      cur <- map[cur, 2]
    }
    count <- count + 1
  }
  cat(count, "\n")
}

solve2 <- function(data) {
  instructions <- strsplit(data[1], "")[[1]]
  map <- lapply(data[3:length(data)], function(l) {
    parts <- strsplit(l, " = ")[[1]]
    targets <- strsplit(substr(parts[2], 2, nchar(parts[2]) - 1), ", ")[[1]]
    list(source = parts[1], targets = targets)
  })
  map <- setNames(lapply(map, `[[`, "targets"), lapply(map, `[[`, "source"))
  map <- t(data.frame(map))
  count <- 0
  cur <- row.names(map)[grepl("A$", row.names(map))]
  # This is such a hack: if a ghost reaches Z at t, it must return to Z at
  # *every* k*t
  loop_count <- rep(0, length(cur))
  while (any(loop_count == 0)) {
    if (instructions[count %% length(instructions) + 1] == "L") {
      cur <- map[cur, 1]
    } else {
      cur <- map[cur, 2]
    }
    count <- count + 1
    reached_end <- grepl("Z$", cur)
    loop_count[reached_end & loop_count[reached_end] == 0] <- count
  }
  cat(as.character(Reduce(lcm.bigz, loop_count)), "\n")
}
