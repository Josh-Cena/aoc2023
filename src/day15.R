string_hash <- function(s) {
  res <- 0
  for (i in seq_len(nchar(s))) {
    ch <- as.integer(charToRaw(substring(s, i, i)))
    res <- ((res + ch) * 17) %% 256
  }
  return(res)
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
  boxes <- vector("list", 256)
  for (part in parts) {
    label <- gsub("[-=].*", "", part)
    label_hash <- string_hash(label)
    ind <- label_hash + 1
    op <- gsub("[^-=]", "", part)
    if (op == "=") {
      focal_length <- as.numeric(gsub(".*=", "", part))
      if (is.null(boxes[[ind]])) {
        boxes[[ind]] <- list()
      }
      boxes[[ind]][[label]] <- focal_length
    } else {
      if (!is.null(boxes[[ind]])) {
        boxes[[ind]][[label]] <- NULL
      }
    }
  }
  powers <- sapply(seq_along(boxes), function(i) {
    box <- boxes[[i]]
    if (is.null(box) || length(box) == 0) {
      return(0)
    }
    return(sum(sapply(seq_along(box), function(j) {
      if (is.null(box[[j]])) {
        return(0)
      }
      return(j * box[[j]])
    })) * i)
  })
  cat(sum(powers), "\n")
}
