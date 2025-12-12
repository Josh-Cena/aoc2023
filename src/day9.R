predict_next <- function(seq) {
  n <- length(seq)
  x <- 1:n
  fit <- lm(seq ~ poly(x, n - 1))
  return(predict(fit, newdata = data.frame(x = n + 1))[[1]])
}

predict_prev <- function(seq) {
  n <- length(seq)
  x <- 1:n
  fit <- lm(seq ~ poly(x, n - 1))
  return(round(predict(fit, newdata = data.frame(x = 0))[[1]]))
}

solve1 <- function(data) {
  data <- lapply(data, function(x) as.numeric(strsplit(x, " ")[[1]]))
  predictions <- lapply(data, predict_next)
  cat(sum(unlist(predictions)), "\n")
}

solve2 <- function(data) {
  data <- lapply(data, function(x) as.numeric(strsplit(x, " ")[[1]]))
  predictions <- lapply(data, predict_prev)
  cat(sum(unlist(predictions)), "\n")
}
