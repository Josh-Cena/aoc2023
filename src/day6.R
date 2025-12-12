# Total distance = (t - x) * x > d
# ==> x^2 - t*x + d < 0
# If t^2 - 4d < 0, can't win
# Otherwise x = [t (+/-) sqrt(t^2 - 4d)] / 2
winning_vals <- function(row) {
  t <- row[["times"]]
  d <- row[["distances"]]
  discriminant <- t^2 - 4 * d
  if (discriminant < 0) {
    print(0)
    return(0)
  } else {
    x1 <- (t - sqrt(discriminant)) / 2
    x2 <- (t + sqrt(discriminant)) / 2
    if (floor(x2) == x2) {
      x2 <- x2 - 0.00001
    }
    if (ceiling(x1) == x1) {
      x1 <- x1 + 0.00001
    }
    return(floor(x2) - ceiling(x1) + 1)
  }
}

solve1 <- function(data) {
  times <- data[1]
  distances <- data[2]
  times <- gsub("Time: +", "", times)
  distances <- gsub("Distance: +", "", distances)
  times <- as.numeric(strsplit(times, " +")[[1]])
  distances <- as.numeric(strsplit(distances, " +")[[1]])
  parsed_data <- data.frame(times = times, distances = distances)
  wins <- apply(parsed_data, 1, winning_vals)
  cat(prod(wins), "\n")
}

solve2 <- function(data) {
  times <- data[1]
  distances <- data[2]
  times <- gsub("Time: +", "", times)
  distances <- gsub("Distance: +", "", distances)
  times <- gsub(" ", "", times)
  distances <- gsub(" ", "", distances)
  times <- as.numeric(times)
  distances <- as.numeric(distances)
  wins <- winning_vals(list(times = times, distances = distances))
  cat(wins, "\n")
}
