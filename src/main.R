source("src/utils.R")

args <- commandArgs(trailingOnly = TRUE)
day <- args[1]
prob <- args[2]
input <- if (length(args) > 2) args[3] else "real"
filename <- paste0("inputs/day", day, "/", input, ".txt")
contents <- readLines(filename, warn = FALSE)

source(paste0("src/day", day, ".R"))
result <- do.call(paste0("solve", prob), list(contents))
