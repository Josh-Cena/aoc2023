library("gsubfn")

solve1 <- function(data) {
  nums <- gsub("[^0-9]", "", data)
  nums <- paste0(substr(nums, 1, 1), substr(nums, nchar(nums), nchar(nums)))
  cat(sum(as.numeric(nums)), "\n")
}

solve2 <- function(data) {
  substitutions <- c(
    zero = "0", one = "1", two = "2", three = "3",
    four = "4", five = "5", six = "6", seven = "7",
    eight = "8", nine = "9"
  )
  pattern <- paste(names(substitutions), collapse="|")
  data <- gsubfn(pattern, function(w) substitutions[[w]], data)
  nums <- gsub("[^0-9]", "", data)
  nums <- paste0(substr(nums, 1, 1), substr(nums, nchar(nums), nchar(nums)))
  print(nums)
  cat(sum(as.numeric(nums)), "\n")
}
