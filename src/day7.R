types <- list(
  "five of a kind" = 6,
  "four of a kind" = 5,
  "full house" = 4,
  "three of a kind" = 3,
  "two pair" = 2,
  "one pair" = 1,
  "high card" = 0
)

hand_type <- function(max_count, uniq_values) {
  if (max_count == 5) {
    types[["five of a kind"]]
  } else if (max_count == 4) {
    types[["four of a kind"]]
  } else if (max_count == 3 && uniq_values == 2) {
    types[["full house"]]
  } else if (max_count == 3) {
    types[["three of a kind"]]
  } else if (max_count == 2 && uniq_values == 3) {
    types[["two pair"]]
  } else if (max_count == 2) {
    types[["one pair"]]
  } else {
    types[["high card"]]
  }
}

compare_rows <- function(strengths) {
  function(row1, row2) {
    if (row1$type > row2$type) {
      return(1)
    } else if (row1$type < row2$type) {
      return(-1)
    }
    cards1 <- unlist(strsplit(row1$hand, ""))
    cards2 <- unlist(strsplit(row2$hand, ""))
    for (i in seq_along(cards1)) {
      if (match(cards1[i], strengths) > match(cards2[i], strengths)) {
        return(1)
      } else if (match(cards1[i], strengths) < match(cards2[i], strengths)) {
        return(-1)
      }
    }
    0
  }
}

solve1 <- function(data) {
  data <- data.frame(matrix(
    unlist(strsplit(data, " ")),
    ncol = 2,
    byrow = TRUE
  ))
  data <- setNames(data, c("hand", "bid"))
  data$bid <- as.numeric(data$bid)
  data$type <- sapply(data$hand, function(hand) {
    card_counts <- table(unlist(strsplit(hand, "")))
    max_count <- max(card_counts)
    hand_type(max_count, length(card_counts))
  })
  data_sorted <- quicksort(
    split(data, seq_len(nrow(data))),
    compare_rows(unlist(strsplit("23456789TJQKA", "")))
  )
  data <- do.call(rbind, data_sorted)
  total_score <- sum(seq_len(nrow(data)) * data$bid)
  cat(total_score, "\n")
}

solve2 <- function(data) {
  data <- data.frame(matrix(
    unlist(strsplit(data, " ")),
    ncol = 2,
    byrow = TRUE
  ))
  data <- setNames(data, c("hand", "bid"))
  data$bid <- as.numeric(data$bid)
  data$type <- sapply(data$hand, function(hand) {
    card_counts <- table(unlist(strsplit(hand, "")))
    j_count <- if ("J" %in% names(card_counts)) card_counts["J"] else 0
    card_counts <- card_counts[names(card_counts) != "J"]
    max_count <- (if (length(card_counts) > 0) max(card_counts) else 0) +
      j_count
    hand_type(max_count, length(card_counts))
  })
  data_sorted <- quicksort(
    split(data, seq_len(nrow(data))),
    compare_rows(unlist(strsplit("J23456789TQKA", "")))
  )
  data <- do.call(rbind, data_sorted)
  total_score <- sum(seq_len(nrow(data)) * data$bid)
  cat(total_score, "\n")
}
