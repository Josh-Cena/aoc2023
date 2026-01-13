string_hash <- function(s) {
  codes <- as.integer(charToRaw(s))
  res <- 0
  for (ch in codes) {
    res <- ((res + ch) * 17) %% 256
  }
  res
}

solve1 <- function(data) {
  line <- data[[1]]
  parts <- strsplit(line, ",")[[1]]
  part_hashes <- sapply(parts, string_hash)
  cat(sum(part_hashes), "\n")
}

solve2 <- function(data) {
  line <- data[[1]]
  parts <- strsplit(line, ",")[[1]]
  # A list of lists, each inner list is a mapping of label to focal length
  boxes <- replicate(256, list(), simplify = FALSE)
  for (part in parts) {
    label <- gsub("[-=].*", "", part)
    label_hash <- string_hash(label)
    ind <- label_hash + 1
    op <- gsub("[^-=]", "", part)
    if (op == "=") {
      focal_length <- as.numeric(gsub(".*=", "", part))
      boxes[[ind]][[label]] <- focal_length
    } else {
      boxes[[ind]][[label]] <- NULL
    }
  }
  powers <- sapply(seq_along(boxes), function(i) {
    box <- boxes[[i]]
    i * sum(unlist(box) * seq_along(box))
  })
  cat(sum(powers), "\n")
}
